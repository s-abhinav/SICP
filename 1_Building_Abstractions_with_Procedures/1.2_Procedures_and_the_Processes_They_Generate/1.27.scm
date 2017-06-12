(define true #t)
(define false #f)

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test-every-a n)
  (define (try-it a)
    (= (expmod a n n) a))

  (define (test-every-a a)
    (cond ((= a n) #t)
	  ((try-it a) (test-every-a (+ 1 a)))
	  (else #f)))
  
  (test-every-a 1))

(fermat-test-every-a 561)
(fermat-test-every-a 1105)
(fermat-test-every-a 1729)
(fermat-test-every-a 2465)
(fermat-test-every-a 2821)
(fermat-test-every-a 6601)

;; All of these tests return true, which show that these numbers indeed fool the
;; Fermat test. Let's try testing these numbers with the prime? procedure below.

(define (prime? n)
  (= n (smallest-divisor n)))

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

(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)

;; This time, these numbers are reported to be not prime.
