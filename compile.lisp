;;;; microlisp.compile:  MicroLisp compiler.

(defpackage microlisp.compile
  (:use :cl
        :microlisp.vocabulary)
  (:export :compile-expanded-expressions))

(in-package :microlisp.compile)

(defparameter *nil-name* "NIL")

(defparameter *value-type-name* "value")
(defparameter *procedure-type-name* "procedure")
(defparameter *cell-type-name* "cell")
(defparameter *symbol-type-name* "symbol")
(defparameter *number-type-name* "number")
(defparameter *character-type-name* "character")

(defparameter *main-function-name* "fmain")
(defparameter *use-function-name* "use")
(defparameter *disuse-function-name* "disuse")
(defparameter *garbage-collect-function-name* "collect_garbage")
(defparameter *free-value-function-name* "free_value")

(defparameter *symbolic=-function-name* "symbolic_equality")

(defparameter *cell-function-name* "new_cell")
(defparameter *first-function-name* "first")
(defparameter *rest-function-name* "rest")

(defparameter *procedure?-function-name* "procedure_p")
(defparameter *cell?-function-name* "cell_p")
(defparameter *symbol?-function-name* "symbol_p")
(defparameter *number?-function-name* "number_p")
(defparameter *character?-function-name* "character_p")

(defparameter *add-function-name* "add_number")
(defparameter *subtract-function-name* "subtract_number")
(defparameter *multiply-function-name* "multiply_number")
(defparameter *divide-function-name* "divide_number")
(defparameter *modulo-function-name* "modulo_number")
(defparameter *numeric=-function-name* "numeric_equality")
(defparameter *numeric>-function-name* "numeric_greater")

(defparameter *character=-function-name* "characteristic_equality")

(defparameter *read-function-name* "read")
(defparameter *write-function-name* "write")
(defparameter *delete-function-name* "delete")

(defparameter *global-environment-name* "global_environment")
(defparameter *procedure-name* "proc")
(defparameter *environment-name* "environment")
(defparameter *arguments-name* "arguments")

(defparameter *default-return-value* 0)

(defparameter *runtime-header-path* "runtime.h")

(defvar *definitions*)
(defvar *parameters*)
(defvar *environment*)
(defvar *procedures*)

(defmacro print-statement (&body body)
  "Terminate a statement printed by BODY."
  `(progn
     ,@body
     (format t ";~%")))

(defmacro print-free-value (&body body)
  "Souround BODY with a *free-value-function-name* call."
  `(print-statement
     (format t "~a((~a *) "
	     *free-value-function-name* *value-type-name*)
    ,@body
    (format t ")")))

(defmacro print-return (&body body)
  "Prepend BODY with a return statement."
  `(print-statement
    (format t "return (~a *) " *value-type-name*)
    ,@body))

(defun print-number (number)
  "Print NUMBER literal."
  (let ((rational (rationalize number)))
    (format t "new_~a(~a, ~a)"
	    *number-type-name*
	    (numerator rational)
	    (denominator rational))))

(defun print-character (character)
  "Print CHARACTER literal."
  (format t "new_~a(~a)"
	  *character-type-name*
	  (char-code character)))

(defun print-string (string)
  "Print STRING literal."
  (loop for character across string do
       (format t "new_~a((~a *) new_~a(~a), "
	       *cell-type-name*
	       *value-type-name*
	       *character-type-name*
	       (char-code character)))
  (format t "~a" *nil-name*)
  (dotimes (x (length string))
    (format t ")")))

(defun print-symbol (symbol)
  "Print SYMBOL literal."
  (if symbol
      (format t "new_~a(\"~a\")"
	      *symbol-type-name* (symbol-name symbol))
      (format t "~a" *nil-name*)))

(defun print-list (list)
  "Print LIST literal."
  (loop for object in list do
       (format t "new_~a((~a *) "
	       *cell-type-name*
	       *value-type-name*)
       (print-value object)
       (format t ", "))
  (format t "~a" *nil-name*)
  (dotimes (x (length list))
    (format t ")")))

(defun print-value (object)
  "Print OBJECT."
  (etypecase object
    (number    (print-number object))
    (character (print-character object))
    (string    (print-string object))
    (symbol    (print-symbol object))
    (cons      (print-list object))))

(defun print-application (function-name type-name object)
  "Print a function application of FUNCTION-NAME with type TYPE-NAME on
OBJECT."
  (format t "~a((~a *) " function-name type-name)
  (print-expression object)
  (format t ")"))

(defun print-application-untyped (function-name object)
  "Print an untyped function application of FUNCTION-NAME on OBJECT."
  (print-application function-name *value-type-name* object))

(defun print-combination (function-name type-name-x type-name-y
			  object-x object-y)
  "Print a function combination of FUNCTION-NAME with types TYPE-NAME-X
and TYPE-NAME-Y on OBJECT-X and OBJECT-Y."
  (format t "~a((~a *) " function-name type-name-x)
  (print-expression object-x)
  (format t ", (~a *) " type-name-y)
  (print-expression object-y)
  (format t ")"))

(defun print-combination-mono (function-name type-name
			       object-x object-y)
  "Print a monotonous function combination of FUNCTION-NAME with type
TYPE-NAME on OBJECT-X and OBJECT-Y."
  (print-combination function-name type-name type-name
		     object-x object-y))

(defun print-symbolic= (object-x object-y)
  "Print symbolic= primitive on OBJECT-X and OBJECT-Y."
  (print-combination-mono *symbolic=-function-name* *symbol-type-name*
			  object-x object-y))

(defun print-cell (first rest)
  "Print cell primitive on FIRST and REST."
  (print-combination *cell-function-name*
		     *value-type-name* *cell-type-name*
		     first rest))

(defun print-first (cell)
  "Print first primitive on CELL."
  (print-application *first-function-name* *cell-type-name* cell))

(defun print-rest (cell)
  "Print rest primitive on CELL."
  (print-application *rest-function-name* *cell-type-name* cell))

(defun print-procedure? (object)
  "Print procedure? primitive on OBJECT."
  (print-application-untyped *procedure?-function-name* object))

(defun print-cell? (object)
  "Print cell? primitive on OBJECT."
  (print-application-untyped *cell?-function-name* object))

(defun print-symbol? (object)
  "Print symbol? primitive on OBJECT."
  (print-application-untyped *symbol?-function-name* object))

(defun print-number? (object)
  "Print number? primitive on OBJECT."
  (print-application-untyped *number?-function-name* object))

(defun print-character? (object)
  "Print character? primitive on OBJECT."
  (print-application-untyped *character?-function-name* object))

(defun print-combination-number (function-name number-x number-y)
  "Print a numeric function combination of FUNCTION-NAME on NUMBER-X and
 NUMBER-Y."
  (print-combination-mono function-name *number-type-name*
			  number-x number-y))

(defun print-numeric= (number-x number-y)
  "Print numeric= primitive on NUMBER-X and NUMBER-Y."
  (print-combination-number *numeric=-function-name* number-x number-y))

(defun print-numeric> (number-x number-y)
  "Print numeric> primitive on NUMBER-X and NUMBER-Y."
  (print-combination-number *numeric>-function-name* number-x number-y))

(defun print-add (number-x number-y)
  "Print add primitive on NUMBER-X and NUMBER-Y."
  (print-combination-number *add-function-name* number-x number-y))

(defun print-subtract (number-x number-y)
  "Print subtract primitive on NUMBER-X and NUMBER-Y."
  (print-combination-number *subtract-function-name* number-x number-y))

(defun print-multiply (number-x number-y)
  "Print multiply primitive on NUMBER-X and NUMBER-Y."
  (print-combination-number *multiply-function-name* number-x number-y))

(defun print-divide (number-x number-y)
  "Print divide primitive on NUMBER-X and NUMBER-Y."
  (print-combination-number *divide-function-name* number-x number-y))

(defun print-modulo (number-x number-y)
  "Print modulo primitive on NUMBER-X and NUMBER-Y."
  (print-combination-number *modulo-function-name* number-x number-y))

(defun print-character= (character-x character-y)
  "Print character= primitive on CHARACTER-X and CHARACTER-Y."
  (print-combination-mono *character=-function-name*
			  *character-type-name*
			  character-x character-y))

(defun print-read (resource)
  "Print read primitive on RESOURCE."
  (print-application *read-function-name* *cell-type-name* resource))

(defun print-write (object resource)
  "Print write primitive on OBJECT and RESOURCE."
  (print-combination *write-function-name*
		     *value-type-name* *cell-type-name*
		     object resource))

(defun print-delete (resource)
  "Print delete primitive on RESOURCE."
  (print-application *delete-function-name* *cell-type-name* resource))

(defun print-cond (cases)
  "Print cond primitive on CASES."
  (if cases
      (let ((condition (first (first cases)))
	    (value     (second (first cases))))
	(format t "(")
	(print-expression condition)
	(format t " ? (~a *) " *value-type-name*)
	(print-expression value)
	(format t " : ")
	(print-cond (rest cases))
	(format t " )"))
      (print-symbol nil)))

(defun procedure-arity (id)
  "Return arity for procedure by ID."
  (length (first (nth id *procedures*))))

(defun print-procedure (id)
  "Print procedure primitive for procedure by ID."
  (format t "new_~a(~a, ~a, f~a, ~a)"
	  *procedure-type-name*
	  *procedure-name*
	  *arguments-name*
	  id
	  (procedure-arity id)))

(defun print-procedure-call (procedure arguments)
  "Print PROCEDURE call with arguments."
  (format t "call_procedure((~a *) " *procedure-type-name*)
  (print-expression procedure)
  (loop for argument in arguments do
       (format t ", (~a *) " *value-type-name*)
       (print-expression argument))
  (format t ")"))

(defun print-call (call)
  "Print CALL."
  (let* ((operator (first call))
	 (arguments (rest call))
	 (argument-1 (first arguments))
	 (argument-2 (second arguments)))
    (case operator

      ;; base axioms
      (quote      (print-value argument-1))
      (symbolic=  (print-symbolic= argument-1 argument-2))

      ;; cell/list axioms
      (cell       (print-cell argument-1 argument-2))
      (first      (print-first argument-1))
      (rest       (print-rest argument-1))
      
      ;; type predicate axioms
      (procedure? (print-procedure? argument-1))
      (cell?      (print-cell? argument-1))
      (symbol?    (print-symbol? argument-1))
      (number?    (print-number? argument-1))
      (character? (print-character? argument-1))

      ;; number calculation axioms
      (numeric=   (print-numeric= argument-1 argument-2))
      (numeric>   (print-numeric> argument-1 argument-2))
      (add        (print-add argument-1 argument-2))
      (subtract   (print-subtract argument-1 argument-2))
      (multiply   (print-multiply argument-1 argument-2))
      (divide     (print-divide argument-1 argument-2))
      (modulo     (print-modulo argument-1 argument-2))

      ;; character equality
      (character= (print-character= argument-1 argument-2))

      ;; read, write and delete
      (read       (print-read argument-1))
      (write      (print-write argument-1 argument-2))
      (delete     (print-delete argument-1))
      
      ;; special forms
      (cond       (print-cond arguments))

      ;; lambda expressions
      (lambda     (print-procedure argument-1))

      ;; procedure call
      (otherwise  (print-procedure-call operator arguments)))))

(defun environment-id (environment name)
  "Return id for NAME in environment."
  (position name environment))

(defun print-environment-value (name)
  "Print environment value for NAME."
  (let ((argument-id (environment-id *parameters* name)))
    (if argument-id
	(format t "~a[~a]" *arguments-name* argument-id)
	(let ((environment-id (environment-id *environment* name)))
	  (if environment-id
	      (format t "~a->~a[~a]"
		      *procedure-name* *environment-name* environment-id)
	      (format t "~a[~a]" *global-environment-name*
		      (or (environment-id *definitions* name)
			  (error "~a" name))))))))

(defun print-expression (expression)
  "Print EXPRESSION."
  (etypecase expression
    ((or number
	 character
	 string)   (print-value expression))
    (cons          (print-call expression))
    (symbol        (print-environment-value expression))))

(defun print-procedure-body (expressions)
  "Print a procedure body for EXPRESSIONS."
  (dolist (expression (butlast expressions))
    (print-free-value (print-expression expression)))
  (print-return (print-expression (first (last expressions)))))

(defun procedure-parameters (procedure)
  "Return parameters of PROCEDURE."
  (first procedure))

(defun procedure-environment (procedure)
  "Return environment of PROCEDURE."
  (second procedure))

(defun procedure-body (procedure)
  "Return body of PROCEDURE."
  (rest (rest procedure)))

(defun print-procedure-function (id procedure)
  "Print declaration for PROCEDURE with ID."
  (format t "static ~a *f~a (struct ~a *~a, ~a **~a) {~%"
          *value-type-name*
          id
          *procedure-type-name*
          *procedure-name*
          *value-type-name*
          *arguments-name*)
  (let ((*parameters* (procedure-parameters procedure))
	(*environment* (procedure-environment procedure)))
    (print-procedure-body (procedure-body procedure)))
  (format t "}~%"))

(defun print-program-initialization (environment-size)
  "Print program initialization for ENVIRONMENT-SIZE."
  (format t "#include <stdlib.h>
#include \"~a\"~%~a *~a[~a];
struct ~a *~a = NIL;
value **arguments = NIL;~%"
          *runtime-header-path*
          *value-type-name*
          *global-environment-name*
          environment-size
	  *procedure-type-name*
	  *procedure-name*))

(defun print-program-main-function (expressions)
  "Print main function with EXPRESSIONS."
  (format t "void ~a (void) {~%"
          *main-function-name*)
  (dolist (expression expressions)
    (print-free-value (print-expression expression)))
  (format t "}~%"))

(defun print-program-kickstart (definitions)
  "Print kickstart for DEFINITIONS."
  (format t "int main (void) {~%")
  (loop for id from 0
     for definition in definitions
     do (format t "~a(~a[~a] = (~a *) "
		*use-function-name*
		*global-environment-name*
		id
		*value-type-name*)
       (print-statement
	 (print-expression (second definition))
	 (format t ")")))
  (format t "~a();~%" *main-function-name*)
  (loop for id from 0
     for definitions in definitions
     do (print-statement
	  (format t "~a(~a[~a])"
		  *disuse-function-name* *global-environment-name* id)))
  (print-statement (format t "~a()" *garbage-collect-function-name*))
  (format t "return ~a;~%}~%" *default-return-value*))

(defun print-program (procedures definitions expressions)
  "Prints program."
  (print-program-initialization (length definitions))
  (loop for id from 0
     for procedure in procedures
     do (print-procedure-function id procedure))
  (print-program-main-function expressions)
  (print-program-kickstart definitions))

(defun nextract-procedures (expanded-expressions)
  "Extract procedures from EXPANDED-EXPRESSIONS, return procedures and
expressions in which procedures are replaced with pointers
(desctructively)."
  (let ((id -1)
	(procedures nil))
    (labels ((pointerize (procedure)
	       (push (append (list (first procedure) *environment*)
			     (rest procedure))
		     procedures)
	       (list (incf id)))
	     (extract-from (expressions)
	       (dolist (expression expressions)
		 (when (consp expression)
		   (cond
		     ((eq 'quote (first expression))
		      nil)
		     ((eq 'lambda (first expression))
		      (let ((*environment* (append (second expression)
						   *environment*)))
			(extract-from (rest (rest expression))))
		      (setf (rest expression)
			    (pointerize (rest expression))))
		     ((eq 'cond (first expression))
		      (dolist (clause (rest expression))
			(when (consp (first clause))
			  (extract-from (first clause)))
			(extract-from (rest clause))))
		     (t
		      (extract-from expression)))))))
      (let ((*environment* nil))
	(extract-from expanded-expressions))
      (values (reverse procedures) expanded-expressions))))

(defun extract-definitions (expanded-expressions)
  "Extract definitions from EXPANDED-EXPRESSIONS, return definitions and
root expressions (really, really stupid)."
  (loop for n from 0
     for expression in expanded-expressions
     unless (and (listp expression)
		 (eq 'define (first expression)))
     return (let ((split-point (- (length expanded-expressions) n)))
	      (values (map 'list (lambda (definition) (rest definition))
			   (butlast expanded-expressions split-point))
		      (last expanded-expressions split-point)))))

(defun initialize (expanded-expressions)
  "Return EXPANDED-EXPRESSIONS prepended with initial definitions."
  (append '((define nil (quote nil))
	    (define t (quote t)))
	  expanded-expressions))

(defun dissect (expanded-expressions)
  "Dissects EXPANDED-EXPRESSIONS into procedures, definitions and
root expressions."
  (multiple-value-bind (procedures expanded-expressions*)
      (nextract-procedures expanded-expressions)
    (multiple-value-bind (definitions root-expressions)
	(extract-definitions expanded-expressions*)
      (values procedures definitions root-expressions))))

(defun definition-names (definitions)
  (loop for definition in definitions collect (first definition)))

(defun compile-expanded-expressions (expanded-expressions
                                     &optional (stream *standard-output*))
  "Compiles EXPANDED-EXPRESSIONS to C output to STREAM."
  (multiple-value-bind (procedures definitions root-expressions)
      (dissect (initialize expanded-expressions))
    (let ((*standard-output* stream)
	  (*definitions* (definition-names definitions))
	  (*parameters* nil)
	  (*environment* nil)
	  (*procedures* procedures))
      (print-program procedures definitions root-expressions))))
