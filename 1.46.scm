;; Iterative improvement procedure.
;; Used by both sqrt and fixed point.
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
	guess
	((iterative-improve good-enough? improve) (improve guess)))))

;; sqrt procedure of 1.1.7

;; Original sqrt procedure
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve x)
  (lambda (guess)
    (average guess (/ x guess))))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (good-enough? x)
  (lambda (guess)
    (< (abs (- (square guess) x)) 0.001)))

(define (iterative-improve-sqrt x guess)
  ((iterative-improve (good-enough? x) (improve x)) guess))

;; (iterative-improve-sqrt 1000000 1.0)
;; => 1000.0000000000118

;; fixed-point procedure of 1.3.3

(define tolerance 0.00001)

;; Original fixed-point procedure
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; (fixed-point cos 1.0)
;; => 0.7390822985224023

;; New procedures for Iterative improve

(define (fixed-point-good-enough? f)
  (lambda (guess)
    (< (abs (- guess (f guess))) tolerance)))

(define (fixed-point-improve f)
  (lambda (guess)
    (f guess)))

(define (iterative-improve-fixed-point f first-guess)
  ((iterative-improve (fixed-point-good-enough? f) (fixed-point-improve f)) first-guess))


;; (iterative-improve-fixed-point cos 1.0)
;; => 0.7390893414033927
;; This value is different from the original procedure because it returns the improved value instead of the value when good-enough is true.
