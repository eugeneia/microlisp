;;;; includes/test:  Assert function.

;; assert:  Returns CONSEQUENCE unless TEST and nil otherwise.
(define assert
    (lambda (consequence t)
      (unless t consequence)))
