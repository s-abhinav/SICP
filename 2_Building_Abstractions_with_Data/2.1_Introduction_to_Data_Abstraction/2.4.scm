(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(car (cons 1 2))
;; => 1

(cdr (cons 1 2))
;; => 2

(car (cons 1 (cons 2 3)))
;; => 1

(car (cdr (cons 1 (cons 2 3))))
;; => 2

(cdr (cdr (cons 1 (cons 2 3))))
;; => 3
