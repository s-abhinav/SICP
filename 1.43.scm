(define (compose f g)
  (lambda (x)
    (f (g x))))

;; Iterative process
(define (repeated f n)
  (define (iter composed n)
    (if (= n 1)
	composed
	(iter (compose composed f) (- n 1))))
  (iter f n))

;; Recursive process
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (square x) (* x x))

;; ((repeated square 2) 5)
;; => 625
