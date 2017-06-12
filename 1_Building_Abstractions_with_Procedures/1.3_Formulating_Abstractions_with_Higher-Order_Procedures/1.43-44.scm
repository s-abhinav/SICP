;; Exercise 1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

;; Exercise 1.43

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

;; Exercise 1.44

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+
	(f (- x dx))
	(f x)
	(f (+ x dx)))
       3)))

(((repeated smooth 5) square) 4) ;; requires revisiting. TODO: find function and plot difference in graph to show smoothing.
