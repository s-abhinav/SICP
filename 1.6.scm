(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; Successfully returns a value
(new-if (= 2 3) 0 5)

;; Successfully returns a value
(new-if (= 1 1) 0 5)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
  guess
  (sqrt-iter (improve guess x))))

;; The new-if will not work in this case because Scheme uses applicative order evaluation.
;; In applicative order evaluation, it will evaluate all arguments before going into the body
;; One of the arguments is (sqr-iter (improve guess x))) where evaluating this argument
;; causes the function to evaluate itself. This will go into an infinite loop.
