;;;; includes/standard:  Utility functions for everybody.

;; equal:  Predicate to test if X and Y are equal.
;; equal:  Two objects are equal if they either satisfy SYMBOLIC=,
;;         NUMERIC=, CHARACTER= or are structurally equivalent cells with
;;         EQUAL members.
(define equal
    (lambda (x y)
      (or (and (cell? x)
	       (cell? y)
	       (and (equal (first x) (first y))
		    (equal (rest x) (rest y))))
	  (and (symbol? x)
	       (symbol? y)
	       (symbolic= x y))
	  (and (number? x)
	       (number? y)
	       (numeric= x y))
	  (and (character? x)
	       (character? y)
	       (character= x y)))))

;; reduce:  Reduce a LIST with FUNCTION.
(define reduce
    (lambda (function list)
      (let ((x (first list))
	    (y (first (rest list)))
	    (rest (rest (rest list))))
	(if rest
	    (reduce function (cell (function x y) rest))
	    (function x y)))))

;; find:  Return first member of LIST that satisfies PREDICATE or nil.
(define find
    (lambda (predicate list)
      (let ((first (first list))
	    (rest (rest list)))
	(if (predicate first)
	    first
	    (when rest (find predicate rest))))))
