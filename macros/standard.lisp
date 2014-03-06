;;;; microlisp.macros.standard:  Macros for everybody.

(defpackage microlisp.macros.standard
  (:use :cl
        :microlisp.vocabulary)
  (:import-from :microlisp.frontend :define-macro)
  (:import-from :microlisp.interpret :lambda-p))

(in-package :microlisp.macros.standard)

;; if:  If CONDITION is not nil evaluate THEN, otherwise evaluate ELSE.
(define-macro if (condition then &optional else)
  `(cond (,condition ,then)
         ((quote t) ,else)))

(defun case-cond (value test-sym cases)
  "Contruct case COND expression for VALUE, TEST-SYM and CASES."
  (let ((value-sym (gensym "value")))
    `(let ((,value-sym ,value))
       (cond ,@(loop for case in cases
                  for key = (car case)
                  for expressions = (cdr case)
                  collect `((,test-sym ,key ,value-sym)
                            ,@expressions))))))

;; case-symbolic=:  Evaluate the first expression group in CASES whose
;; key is SYMBOLIC= to VALUE. If no key is SYMBOLIC= to value return
;; NIL.
(define-macro case-symbolic= (value &rest cases)
  (case-cond value 'symbolic=
             (loop for case in cases
                collect (cons `(quote ,(car case)) (cdr case)))))

;; case-numeric=:  Evaluate the first expression group in CASES whose
;; key is NUMERIC= to VALUE. If no key is NUMERIC= to value return NIL.
(define-macro case-numeric= (value &rest cases)
  (case-cond value 'numeric= cases))

;; case-character=:  Evaluate the first expression group in CASES whose
;; key is CHARACTER= to VALUE. If no key is CHARACTER= to value return
;; NIL.
(define-macro case-character= (value &rest cases)
  (case-cond value 'character= cases))

;; not:  Negate OBJECT.
(define-macro not (object)
  `(if ,object
       nil
       (quote t)))

;; when:  If CONDITION is not nil evaluate BODY.
(define-macro when (condition &rest body)
  `(cond (,condition ,@body)))

;; unless:  If CONDITION is nil evaluate BODY.
(define-macro unless (condition &rest body)
  `(cond ((not ,condition) ,@body)))

(defun list-macro (items)
  "Returns nested cell expressions for ITEMS."
  (let ((item (first items))
	(rest (rest items)))
    `(cell ,item ,(when rest (list-macro rest)))))

;; list:  Build a list consisting of ITEMS.
(define-macro list (&rest items)
  (list-macro items))

;; let:  Evaluate BODY inside lambda with BINDINGS.
(define-macro let (bindings &rest body)
  `((lambda ,(loop for binding in bindings collect
		  (let ((name (first binding)))
		    (if (symbolp name)
			name
			(error "Invalid name in binding ~a." binding))))
      ,@body)
    ,@(loop for binding in bindings
         collect (second binding))))

(defun and-macro (objects)
  "Returns nested let/cond expressions with logical 'and' functionality
for OBJECTS."
  (let ((object (first objects))
	(rest (rest objects)))
    `(let ((result ,object))
       (cond (result ,(if rest (and-macro rest) 'result))))))

;; and:  Logically 'and' concatenate OBJECTS.
(define-macro and (&rest objects)
  (and-macro objects))

(defun or-macro (objects)
  "Returns nested let/cond expressions with logical 'or' functionality
for OBJECTS."
  (let ((object (first objects))
	(rest (rest objects)))
    `(let ((result ,object))
       (cond (result result)
             (t ,(when rest (or-macro rest)))))))

;; or:  Logically 'or' concatenate OBJECTS.
(define-macro or (&rest objects)
  (or-macro objects))

;; +:  Reduce NUMBERS with 'add'.
(define-macro + (&rest numbers)
  (when (> 2 (length numbers))
    (error "+ called with less than two NUMBERS."))
  `(reduce (lambda (number-a number-b) (add number-a number-b))
	   (list ,@numbers)))

;; -:  Reduce NUMBERS with 'subtract'.
(define-macro - (&rest numbers)
  (when (> 2 (length numbers))
    (error "- called with less than two NUMBERS."))
  `(reduce (lambda (number-a number-b) (subtract number-a number-b))
	   (list ,@numbers)))

;; *:  Reduce NUMBERS with 'multiply'.
(define-macro * (&rest numbers)
  (when (> 2 (length numbers))
    (error "* called with less than two NUMBERS."))
  `(reduce (lambda (number-a number-b) (multiply number-a number-b))
	   (list ,@numbers)))

;; /:  Reduce NUMBERS with 'divide'.
(define-macro / (&rest numbers)
  (when (> 2 (length numbers))
    (error "/ called with less than two NUMBERS."))
  `(reduce (lambda (number-a number-b) (divide number-a number-b))
	   (list ,@numbers)))

;; numeric<:  Test if NUMBER-B is greater than NUMBER-A.
(define-macro numeric< (number-a number-b)
  `(numeric> ,number-b ,number-a))

(defun linear-predicate-macro (predicate arguments)
  "Test if ARGUMENTS satisfy PREDICATE in a linear way."
  (when (> 2 (length arguments))
    (error "linear-predicate-macro called with less than two NUMBERS."))
  `(and ,@(loop
	     for a = (first arguments) then (first rest)
	     for rest = (rest arguments) then (rest rest)
	     until (not rest)
	     collect `(,predicate ,a ,(first rest)))))

;; >:  Test if NUMBERS are decreasing in a linear way.
(define-macro > (&rest numbers)
  (linear-predicate-macro 'numeric> numbers))

;; <:  Test if NUMBERS are increasing in a linear way.
(define-macro < (&rest numbers)
  (linear-predicate-macro 'numeric< numbers))

;; y-combinate:  Use the Y combinator to return the fixed point of LAMBDA
;;               calling itself by NAME.
(define-macro y-combinate (name lambda)
  (unless (symbolp name)
    (error "~a is not a valid NAME." name))
  (unless (lambda-p lambda)
    (error "~a is not a valid LAMBDA." lambda))
  (let ((parameters (second lambda)))
    `((lambda (f)
	((lambda (x) (x x))
	 (lambda (y)
	   (f (lambda ,parameters
		((y y) ,@parameters))))))
      (lambda (,name)
	,lambda))))

;; y-let:  Evaluate BODY in a 'let' expression where BINDINGS are bound
;;         to fixed points from 'y-combinate'.
(define-macro y-let (bindings &rest body)
  (unless bindings
    (error "Y-LET called without BINDINGS."))
  `(let ,(loop for binding in bindings collect
	      (let ((name (first binding))
		    (lambda (second binding)))
		`(,name (y-combinate ,name ,lambda))))
     ,@body))

;; map:  Map FUNCTION over LISTS.
(define-macro map (function &rest lists)
  (unless lists
    (error "Map macro called without LISTS."))
  (let ((gensyms (loop for list in lists collect (gensym))))
    `(y-let ((mapper (lambda (function ,@gensyms)
		       (when (and ,@gensyms)
			 (cell (function ,@(loop for sym in gensyms
					      collect `(first ,sym)))
			       (mapper function
				       ,@(loop for sym in gensyms
					    collect `(rest ,sym))))))))
	    (mapper ,function ,@lists))))
