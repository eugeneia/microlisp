;;;; test/axioms:  Test MicroLisp axioms for correctness.
;;;; uses:  macros/test.

(define axiom-test-ran t)

(test
 ;; atoms
 (assert "T does not evaluate to itself."
	 (symbolic= t (quote t)))
 (assert "NIL does not evaluate to itself."
	 (symbolic= nil (quote nil)))

 ;; cells
 (assert "cell fails."
         (equal (quote (1 2)) (cell 1 (cell 2 nil))))
 (assert "first fails."
	 (numeric= 42 (first (cell 42 nil))))
 (assert "rest fails."
	 (numeric= 42 (first (rest (cell 1 (cell 42 nil))))))

 ;; types
 (assert "symbol? fails."
	 (and (symbol? (quote foo))
              (not (symbol? (cell nil nil)))))
 (assert "procedure? fails."
	 (and (procedure? (lambda (x) x))
              (not (procedure? (cell nil nil)))))
 (assert "cell? fails."
	 (and (cell? (cell nil nil))
              (not (cell? (quote foo)))))
 (assert "number? fails."
	 (and (number? 42)
              (not (number? (quote foo)))))
 (assert "character? fails."
	 (and (character? #\c)
              (not (character? (quote foo)))))

 ;; numbers
 (assert "numeric= fails."
	 (and (numeric= 42 42)
              (not (numeric= 42 43))))
 (assert "numeric> fails."
	 (and (numeric> 2 1)
	      (not (numeric> 1 1))
	      (not (numeric> 1 2))))
 (assert "add fails."
	 (numeric= (add 1 2) 3))
 (assert "subtract fails."
	 (numeric= (subtract 1 2) -1))
 (assert "multiply fails."
	 (numeric= (multiply 2 2) 4))
 (assert "divide fails."
	 (numeric= (divide 1 2) 1/2))
 (assert "modulo fails."
	 (numeric= (modulo 12 5) 2))

 ;; characters
 (assert "character= does not evaluate to itself."
	 (and (character= #\C #\C)
              (not (character= #\c #\C))))

 ;; read, write and delete
 (assert "write fails."
	 (write 42 (quote ("foo"))))
 (assert "read fails."
	 (numeric= 42 (read (quote ("foo")))))
 (assert "delete fails."
	 (and (delete (quote ("foo")))
	      (not (read (quote ("foo"))))))

 ;; cond
 (assert "cond fails."
	 (and (cond (nil nil) (t t) (t nil))
              (not (cond (nil nil)))))

 ;; define
 (assert "define fails."
	 axiom-test-ran)

 ;; lambda
 (assert "lambda does not evaluate properly / procedure call fails."
	 (let ((lam (lambda (x) x)))
	   (lam t)))
 (assert "lexical nesting fails."
	 (equal
          ((lambda (x)
             ((lambda (y)
                ((lambda (z)
                   (cell x (cell y (cell z nil))))
                 y))
              x))
           42)
          (quote (42 42 42))))
 (assert "lexical shadowing fails."
	 (equal ((lambda (x) (cell x (cell ((lambda (x) x) 42) nil))) 7)
                (quote (7 42)))))
