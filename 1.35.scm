;; The golden ratio satisfies the equation
;; phi^2 = phi + 1
;; Then
;; phi = (phi + 1) / phi
;; which is
;; phi = phi/phi + 1/phi
;; which is
;; phi = 1 + 1/phi

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

(define phi
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

phi

;; => 1.6180327868852458
