;;;; microlisp.interpret:  MicroLisp interpreter.

(defpackage microlisp.interpret
  (:use :cl
        :microlisp.vocabulary)
  (:shadow :variable)
  (:export :*environment*
	   :evaluate-expanded-expression
	   :lambda-p))

(in-package :microlisp.interpret)

(defvar *environment* nil
  "Initial environment.")

(defun variable (name value)
  "Returns variable NAME with VALUE."
  (list name value))

(defun variable-name (variable)
  "Returns name of VARIABLE."
  (first variable))

(defun variable-value (variable)
  "Returns value of VARIABLE."
  (second variable))

(defun environment-variable (name environment)
  "Returns variable for NAME in ENVIRONMENT."
  (or (assoc name environment)
      (assoc name *environment*)))

(defun environment-value (name environment)
  "Returns value for NAME in ENVIRONMENT."
  (let ((variable (environment-variable name environment)))
    (if variable
	(variable-value variable)
	(error "~a is not defined." name))))

(defun set-global-environment-value (name value)
  "Set variable NAME in *environment* to VALUE."
  (cond ((environment-variable name *environment*)
	 (format *error-output* "WARNING: Redefining ~a.~%" name)
	 (rplacd (assoc name *environment*) (list value)))
	(t
	 (push (variable name value) *environment*))))

(defun evaluate-atom (atom environment)
  "Evaluate ATOM in ENVIRONMENT."
  (cond ((or (numberp atom)
	     (characterp atom)) atom)
	((stringp atom)         (coerce atom 'list))
	(t                      (environment-value atom environment))))

(defun call-operator (call)
  "Returns operator of CALL."
  (first call))

(defun call-arguments (call)
  "Returns arguments of CALL."
  (rest call))

(defun path-path (path)
  "Coerces list of list of characters PATH to list of strings."
  (map 'list (lambda (string) (coerce string 'string)) path))

(defun path-pathname (path)
  "Returns pathname designated by path."
  (let ((string-path (path-path path)))
    (merge-pathnames
     (make-pathname :directory (cons :relative (butlast string-path))
		    :name (first (last string-path))))))

(defun symbol-equal (a b)
  "symbolic= equivalent."
  (unless (symbolp a)
    (error "Not a symbol: ~a" a))
  (unless (symbolp b)
    (error "Not a symbol: ~a" b))
  (eq a b))

(defun cons-list (first rest)
  "cell equivalent."
  (unless (or (not rest)
	      (consp rest))
    (error "Not a cons or nil: ~a" rest))
  (cons first rest))

(defun read-object (resource)
  "Returns object read from RESOURCE or nil."
  (handler-case
      (if (consp resource)
	  (with-open-file (in (path-pathname resource)
			      :if-does-not-exist nil)
	    (when in
	      (read in nil)))
	  (read *standard-input* nil))
    (file-error ())))

(defun ml-string-p (object)
  "Predicate to test if OBJECT is a MicroLisp string."
  (and (listp object)
       (dolist (item object t)
	 (unless (characterp item) (return)))))

(defun write-object (object resource)
  "Writes OBJECT to resource, returns nil on failure."
  (handler-case
      (if (consp resource)
	  (let ((pathname (path-pathname resource)))
	    (ensure-directories-exist pathname)
	    (with-open-file (out pathname
				 :direction :output
				 :if-exists :supersede)
	      (prin1 object out)
	      t))
	  (progn (if (ml-string-p object)
		     (prin1 (coerce object 'string) *standard-output*)
		     (prin1 object *standard-output*))
		 (terpri)
		 t))
    (file-error ())))

(defun delete-resource (path)
  "Deletes resource at PATH."
  (delete-file (path-pathname path)))

(defun case-condition (case)
  "Returns condition of CASE."
  (first case))

(defun case-value (case)
  "Returns value of CASE."
  (second case))

(defun evaluate-condition-cases (cases environment)
  "Evaluate condition CASES in ENVIRONMENT."
  (let ((case (first cases)))
    (if (evaluate (case-condition case) environment)
	(evaluate (case-value case) environment)
	(when (rest cases)
	  (evaluate-condition-cases (rest cases) environment)))))

(defun definition-name (definition)
  "Returns name of DEFINITION."
  (first definition))

(defun definition-value (definition)
  "Returns value of DEFINITION."
  (second definition))

(defun evaluate-define (definition environment)
  "Evaluate DEFINITION in ENVIRONMENT."
  (let ((name (definition-name definition))
	(value (evaluate (definition-value definition)
			 environment)))
    (set-global-environment-value name value))
  (values))

(defun lambda-list (lambda-expression)
  "Returns lambda-list of LAMBDA-EXPRESSION."
  (first (rest lambda-expression)))

(defun lambda-body (lambda-expression)
  "Returns body of LAMBDA-EXPRESSION."
  (rest (rest lambda-expression)))

(defun lambda-list-p (object)
  "Predicate to test if OBJECT is a proper lambda-list."
  (and (listp object)
       (dolist (item object t)
	 (unless (symbolp item) (return)))))

(defun lambda-p (object)
  "Predicate to test if OBJECT is a proper lambda-expression."
  (and (listp object)
       (eq 'lambda (first object))
       (lambda-list-p (lambda-list object))))

(defun make-procedure (lambda-expression environment)
  "Return new procedure consisting of LAMBDA-EXPRESSION and ENVIRONMENT."
  (unless (lambda-p lambda-expression)
    (error "~a is not a valid lambda expression." lambda-expression))
  (list 'procedure
	lambda-expression
	environment))

(defun procedure-lambda-expression (procedure)
  "Return lambda expression for PROCEDURE."
  (second procedure))

(defun procedure-environment (procedure)
  "Return environment for PROCEDURE."
  (third procedure))

(defun procedure-p (object)
  "Predicate to test if OBJECT is a procedure."
  (and (listp object)
       (eq 'procedure (first object))))

(defun return-value (values)
  "Returns last value from VALUES."
  (first (last values)))

(defun map-variables (lambda-list arguments)
  "Return alist mapping LAMBDA-LIST to ARGUMENTS."
  (map 'list (lambda (name value) (variable name value))
       lambda-list arguments))

(defun evaluate-procedure-call (procedure arguments)
  "Evaluate call to PROCEDURE with ARGUMENTS."
  (unless (procedure-p procedure)
    (error "~a is not a valid procedure." procedure))
  (let ((parameters
	 (lambda-list (procedure-lambda-expression procedure)))
	(body
	 (lambda-body (procedure-lambda-expression procedure)))
	(environment (procedure-environment procedure)))
    (return-value
     (evaluate-list
      body
      (append (map-variables parameters arguments)
	      environment)))))

(defun evaluate-call (call environment)
  "Evaluate CALL in ENVIRONMENT."
  (let ((call-operator (call-operator call))
	(call-arguments (call-arguments call)))
    (case call-operator
      ;; base axioms
      (quote     (first call-arguments))
      (symbolic= (symbol-equal (evaluate
				(first call-arguments) environment)
			       (evaluate
				(second call-arguments) environment)))

      ;; cell/list axioms
      (cell  (cons-list (evaluate
			 (first call-arguments) environment)
			(evaluate
			 (second call-arguments) environment)))
      (first (first (evaluate
		     (first call-arguments) environment)))
      (rest  (rest (evaluate
		    (first call-arguments) environment)))
      
      ;; type predicate axioms
      (procedure? (procedure-p (evaluate
				(first call-arguments) environment)))
      (cell?      (consp (evaluate
			  (first call-arguments) environment)))
      (symbol?    (symbolp (evaluate
                            (first call-arguments) environment)))
      (number?    (numberp (evaluate
			    (first call-arguments) environment)))
      (character? (characterp (evaluate
			       (first call-arguments) environment)))

      ;; number calculation axioms
      (numeric= (= (evaluate
		    (first call-arguments) environment)
		   (evaluate
		    (second call-arguments) environment)))
      (numeric> (> (evaluate
		    (first call-arguments) environment)
		   (evaluate
		    (second call-arguments) environment)))
      (add      (+ (evaluate
		    (first call-arguments) environment)
		   (evaluate
		    (second call-arguments) environment)))
      (subtract (- (evaluate
		    (first call-arguments) environment)
		   (evaluate
		    (second call-arguments) environment)))
      (multiply (* (evaluate
		    (first call-arguments) environment)
		   (evaluate
		    (second call-arguments) environment)))
      (divide   (/ (evaluate
		    (first call-arguments) environment)
		   (evaluate
		    (second call-arguments) environment)))
      (modulo   (mod (evaluate
		      (first call-arguments) environment)
		     (evaluate
		      (second call-arguments) environment)))

      ;; character equality
      (character= (char= (evaluate
                          (first call-arguments) environment)
                         (evaluate
                          (second call-arguments) environment)))

      ;; read, write and delete
      (read   (read-object (evaluate
			    (first call-arguments) environment)))
      (write  (write-object (evaluate
			     (first call-arguments) environment)
			    (evaluate
			     (second call-arguments) environment)))
      (delete (delete-resource (evaluate
				(first call-arguments) environment)))
      
      ;; special forms
      (cond   (evaluate-condition-cases
	       call-arguments environment))
      (define (evaluate-define
	       call-arguments environment))

      ;; lambda call
      (lambda (make-procedure call environment))

      ;; procedure call
      (otherwise (evaluate-procedure-call
		  (evaluate call-operator environment)
		  (evaluate-list call-arguments environment))))))

(defun evaluate-list (expressions environment)
  "Evaluate EXPRESSIONS in ENVIRONMENT."
  (map 'list (lambda (expression) (evaluate expression environment))
       expressions))

(defun evaluate (expression environment)
  "Evaluate MicroLisp expression EXPRESSION in environment ENVIRONMENT."
  (if (atom expression)
      (evaluate-atom expression environment)
      (evaluate-call expression environment)))

(defun evaluate-expanded-expression
    (expanded-expression &optional (environment *environment*))
  "Evaluate MicroLisp EXPANDED-EXPRESSION in ENVIRONMENT."
  (evaluate expanded-expression environment))

(defun trace-evaluation ()
  "Trace evaluation."
  (trace evaluate evaluate-atom evaluate-call evaluate-condition-cases
	 evaluate-define evaluate-expanded-expression
	 evaluate-procedure-call evaluate-list))

(defun untrace-evaluation ()
  "Untrace evaluation."
  (untrace evaluate evaluate-atom evaluate-call evaluate-condition-cases
	   evaluate-define evaluate-expanded-expression
	   evaluate-procedure-call evaluate-list))

(set-global-environment-value nil nil)
(set-global-environment-value t t)
