;;;; macros/test:  Test macro.

(defpackage microlisp.macros.test
  (:use :cl)
  (:import-from :microlisp.frontend :define-macro))

(in-package :microlisp.macros.test)

(define-macro test (&rest exercises)
  `(let ((result (or ,@exercises)))
     (when result
       (write result nil))))
