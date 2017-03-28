(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))

;; In applicative order, the operands will be evaluated first, then applied
;; to the operator. In this case, for (test 0 (p)), p will return p, eventually
;; going into an infinite loop.

;; In normal order, the operator gets evaluated first until it returns only
;; primitive procedures, then these procedures get applied to the operands.
;; In the case of (test 0 (p)), it will be expanded into:
(if (= x 0) 0 y)
;; considering if and = to be primitive procedures,
;; (= x 0) will be evaluated which results in true
(if #t 0 y)
;; where 0 is the final result of the test procedure for (test 0 (p))
