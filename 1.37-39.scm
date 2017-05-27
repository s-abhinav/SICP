;; Exercise 1.37

(define (cont-frac n d k)
  (define (recurse i)
    (if (= i k)
	(/ (n i) (d i))
	(/ (n i) (+ (d i) (recurse (+ i 1))))))
  (recurse 1))

;; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)
;; => 0.6180339887498948

(define (cont-frac-iter n d k)
  (define (iter k result)
    (if (= k 1)
	(/ (n k) (+ (d k) result))
	(iter (- k 1) (/ (n k) (+ (d k) result)))))
  (iter k 0))

;; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10000)
;; => "VM: Stack overflow"

;; (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10000)
;; => 0.6180339887498948


;; Exercise 1.38
(define (euler-expansion-helper a b c k)
  (cond ((= k 1) a)
	((= k 2) b)
	((= k 3) c)
	(else
	 (euler-expansion-helper
	  b
	  c
	  (if (= a 1) a (+ a 2))
	  (- k 1)
	  ))
	))

(define (euler-expansion k)
  (euler-expansion-helper 1 2 1 k))

;; (cont-frac-iter (lambda (i) 1.0) euler-expansion 1000)
;; => 0.7182818284590453 which is e - 2

;; Exercise 1.39
(define (square x) (* x x))

(define (tan-n x)
  (lambda (k)
    (if (= k 1)
	x
	(- (square x)))))

(define (tan-d k)
  (- (* k 2) 1))

(define (tan-cf x k)
  (cont-frac-iter (tan-n x) tan-d k))

;; (tan-cf 0.5 1000)
;; 0.5463024898437905
