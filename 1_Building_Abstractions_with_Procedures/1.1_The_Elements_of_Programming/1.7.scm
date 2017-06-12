(define (square x) (* x x))

(define (sqrt-iter guess x)
  (begin
    (display guess)
    (newline)
    (if (good-enough-optimized? guess x)
	guess
	(sqrt-iter (improve guess x) x))))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (good-enough-optimized? guess x)
  (< (abs (- (square guess) x)) (/ x 1000000)))

(define (sqrt x)
  (sqrt-iter 1.0 x))


;; When the number is very large, for instance 10000000000000, the value of (improve guess x)
;; never changes as average calculation results in the same value. The value of guess never changes, which means the sqrt function goes into an infinite loop.
;; One way to avoid this problem is to make the precision (0.001) dependent on the value of x.

