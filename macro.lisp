;;;; microlisp.macro:  Define and expand macros for MicroLisp code.

(defpackage microlisp.macro
  (:use :cl)
  (:export :macro
	   :find-macro
           :expand-expression
	   :expand-expressions))

(in-package :microlisp.macro)

(defmacro macro (name lambda-list &body body)
  "Make macro NAME with LAMBDA-LIST and BODY."
  `(list ,name
         (lambda ,lambda-list ,@body)))

(defun find-macro (name macros)
  "Find macro by NAME in MACROS."
  (assoc name macros :test #'equal))

(defun find-macro-function (name macros)
  "Find macro function by NAME in MACROS."
  (second (find-macro name macros)))

(defun expand-expression (expression macros)
  "Expand MACROS in EXPRESSION and return expanded expression."
  (if (and expression
	   (listp expression)
	   (not (eq 'quote (first expression)))) ; QUOTE
      (let ((operator (first expression)))
	(if (symbolp operator)
	    (let ((arguments
		   (expand-expressions (rest expression) macros))
		  (macro
		      (find-macro-function (symbol-name operator)
					   macros)))
	      (if macro
		  (expand-expression (apply macro arguments) macros)
		  (cons operator arguments)))
	    (expand-expressions expression macros)))
      expression))

(defun expand-expressions (expressions macros)
  "Expand MACROS in EXPRESSIONS and return expanded expressions."
  (when expressions
    (let ((rest (rest expressions)))
      (cons (expand-expression (first expressions) macros)
            (when rest (expand-expressions rest macros))))))
