(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (inc n) (+ n 1))

(define (factorial n)
  (product identity 1 inc n))

;; (factorial 5)

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (cube x)
  (* x x x))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (* (cond ((= k 0) 1)
	     ((= k n) 1)
	     ((odd? k) 4)
	     ((even? k) 2))
       (f (+ a (* k h)))))
  (*
   (/ h 3)
   (sum y 0 inc n)))

;; (simpsons-rule cube 0 1 1000)
