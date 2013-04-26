;;;; microlisp.frontend:  MicroLisp frontend.
;;;; microlisp:           MicroLisp development environment.

(defpackage microlisp.frontend
  (:use :cl
	:microlisp.parse
	:microlisp.macro
	:microlisp.interpret
        :microlisp.compile)
  (:shadow :compile-files)
  (:export :define-macro
	   :compile-files
           :evaluate
	   :include))

(defpackage microlisp
  (:use :cl
        :microlisp.frontend
        :microlisp.vocabulary)
  (:shadowing-import-from :microlisp.frontend :compile-file))

(in-package :microlisp.frontend)

(defvar *macros* nil
  "Macro table.")

(defmacro define-macro (name lambda-list &body body)
  "Define a macro."
  (let ((macro-name (symbol-name name)))
    `(let* ((macro (macro ,macro-name ,lambda-list ,@body))
            (old-macro (find-macro ,macro-name *macros*)))
       (cond (old-macro
              (format *error-output* "WARNING: Redefining macro ~a.~%"
                      ,macro-name)
              (rplacd old-macro (list (second macro))))
             (t
              (push macro *macros*))))))

(defun newline-stream ()
  "Returns stream containing #\Newline."
  (make-string-input-stream (string #\Newline)))

(defun concatenate-files (files)
  "Open FILES and return concatenated stream."
  (let ((first (first files))
	(rest (rest files)))
    (if rest
	(make-concatenated-stream (open first)
				  (newline-stream)
				  (concatenate-files rest))
	(make-concatenated-stream (open first)))))

(defmacro with-open-files ((var files) &body body)
  "Open FILES and bind concatenated stream to var."
  `(let ((,var (concatenate-files ,files)))
     (unwind-protect
	  (progn ,@body)
       (dolist (stream (concatenated-stream-streams ,var))
	 (close stream)))))

(defun compile-files (&rest files)
  "Compile MicroLisp source file to C."
  (unless files
    (error "No input FILES given."))
  (let ((output-file (merge-pathnames (make-pathname :type "c")
				      (first (last files)))))
    (with-open-files (in files)
      (with-open-file (out output-file
			   :direction :output
			   :if-exists :supersede)
	(compile-expanded-expressions
	 (expand-expressions (parse in) *macros*)
	 out)))))

(defun evaluate (expression &optional (environment *environment*))
  "Evaluate MicroLisp EXPRESSION in ENVIRONMENT."
  (evaluate-expanded-expression (expand-expression expression *macros*)
				environment))

(defun include (pathname &optional (environment *environment*))
  "Evaluate MicroLisp file at PATHNAME in ENVIRONMENT."
  (let ((expressions (with-open-file (input pathname) (parse input))))
    (dolist (expression expressions)
      (evaluate expression environment)))
  (values))
