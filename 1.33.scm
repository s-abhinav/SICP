(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (inc n) (+ 1 n))

(define (filtered-accumulate combiner predicate? null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (predicate? a) (term a) null-value)
		(filtered-accumulate combiner predicate? null-value term (next a) next b))))

(filtered-accumulate + prime? 0 square 1 inc 11)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define n 9)

(define (relatively-prime? a)
  (if (= 1 (gcd a n))
      #t
      #f))

(filtered-accumulate * relatively-prime? 1 identity 1 inc (- n 1))
