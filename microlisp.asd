;;;; microlisp-asd:  ASD for microlisp.

(defpackage microlisp-asd
  (:use :cl :asdf))

(in-package :microlisp-asd)

(defsystem microlisp
  :components ((:file "parse")
	       (:file "macro")
	       (:file "vocabulary")
	       (:file "compile"
                :depends-on ("vocabulary"))
	       (:file "interpret"
                :depends-on ("vocabulary"))
	       (:file "micro"
		:depends-on ("parse"
			     "macro"
			     "compile"
			     "interpret"
			     "vocabulary"))))
