(define tolerance 0.00001)

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

(define (average x y)
  (/ (+ x y) 2.0))

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter composed n)
    (if (= n 1)
	composed
	(iter (compose composed f) (- n 1))))
  (iter f n))

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+
	(f (- x dx))
	(f x)
	(f (+ x dx)))
       3)))

;; (fixed-point (lambda (x) (/ (log 1000) (log x))) 10)

(define (sqrt x)
  (fixed-point 
   (average-damp 
    (lambda (y) (/ x y)))
   1.0))

(sqrt 25)

(expt 2 3)

(define (nrt x n d)
  (fixed-point
   ((repeated average-damp d)
    (lambda (y)
      (/ x (expt y (- n 1)))))
   1.0))

;; (nrt 512 9 4)
;; => 2.0000074709755573

;; (nrt 1000000000000000000000000000000000000000000000000000 51 6)
;; => 10.0000019684269

;; (nrt 128 7 4)
;; => 2.0000106805408264

;; (nrt 64 6 1)
;; => 2.0000050404244165

;; (nrt 1048576 20 6)
;; => 2.000019826846883

;; (nrt 1073741824 30 8)
;; => 2.000071064198754
