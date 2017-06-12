(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))


(define (inc n) (+ n 1))


(define (factorial n)
  (product identity 1 inc n))

(define (pi/4 n)
  (product valuen 0 inc n))

(define (valuen x)
  (cond ((even? x) (/ (+ x 2) (+ x 3)))
	(else (/ (+ x 3) (+ x 2)))))

;; (* 4.0  (pi/4 100000)) => 3.1415769461370178


