;; Exercise 1.22

(define (square x) (* x x))

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

(define (runtime) (current-time))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start range)
  (cond
   ((and (not (= 0 range)) (even? start))
    (search-for-primes (+ 1 start) range))
   ((and (not (= 0 range)) (prime? start))
    (display start)
    (newline)
    (search-for-primes (+ 2 start) (- range 1)))
   ((and (not (= 0 range)))
    (search-for-primes (+ 2 start) range))
   (else '())))

(use-modules (statprof))
(statprof (search-for-primes 1000 3))

(define (start-search-for-prime start-time start range)
  (search-for-primes start range)
  (display (- (runtime) start-time)))

; (start-search-for-prime (runtime) 90000000000000 10) ; => 17 seconds

;; Exercise 1.23

(define (next input)
  (if (= input 2)
      3
      (+ 2 input)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (next test-divisor)))))

; (start-search-for-prime (runtime) 90000000000000 10); => 11 seconds
;; not quite twice as fast but almost there
;; It is different than 2 because next computes the value of input conditionally each time.

;; Exercise 1.24

;; To be able to analyse the results of 1.22, there needs to be a way to count in micro / milliseconds in Guile
