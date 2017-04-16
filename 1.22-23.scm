;; Exercise 1.22

;; Guile has a pair structure for returning the time in seconds and microseconds
;; hence the implementation of a difference procedure for the pair.
(define (guile-time-of-day-difference time-end time-start)
  (+
   (- (car time-end) (car time-start))
   (-
    (* (cdr time-end) (expt 10.0 -6))
    (* (cdr time-start) (expt 10.0 -6)))))

;; Define runtime as gettimeofday
(define (runtime) (gettimeofday))

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

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (guile-time-of-day-difference (runtime) start-time))))

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

(define (start-search-for-prime start-time start range)
  (search-for-primes start range)
  (display (guile-time-of-day-difference (runtime) start-time)))

;; (start-search-for-prime (runtime) 1000 3) ; => 30 microseconds
;; (start-search-for-prime (runtime) 10000 3) ; => 99
;; (start-search-for-prime (runtime) 100000 3) ; => 236
;; (start-search-for-prime (runtime) 1000000 3) ; => 559
;; (start-search-for-prime (runtime) 900000000000000 3) ; => 14 seconds

;; => (* 30 (sqrt 10)) which is 94.8683 microseconds 
;; How about 100000?
;; => (* 99 (sqrt 10)) which is 313.0 microseconds
;; How about 1000000?
;; => (* 236 (sqrt 10)) which is 746.297 microseconds
;; It seems that the time taken follows more or less the previous value times
;; the square root of 10.

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

;; (start-search-for-prime (runtime) 1000 3) ; => 28 microseconds
;; (start-search-for-prime (runtime) 10000 3) ; => 70
;; (start-search-for-prime (runtime) 100000 3) ; => 183
;; (start-search-for-prime (runtime) 1000000 3) ; => 354
;; (start-search-for-prime (runtime) 900000000000000 3) => 9 seconds - the improvement is by
;; a factor of around 1.5 times

;; The introduction of the improved find-divisor does not really improve the
;; speed of execution of the procedure by 2, but by some other value less than 2.
;; This might be explained from the fact that the improved find-divisor procedure
;; uses the next procedure which computes the next value based on the input value,
;; which eventually takes more procedures to be executed.

;; Exercise 1.24

(define true #t)
(define false #f)

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

(define (search-for-fast-primes start times range)
  (cond
   ((and (not (= 0 range)) (even? start))
    (search-for-fast-primes (+ 1 start) times range))
   ((and (not (= 0 range)) (fast-prime? start times))
    (display start)
    (newline)
    (search-for-fast-primes (+ 2 start) times (- range 1)))
   ((and (not (= 0 range)))
    (search-for-fast-primes (+ 2 start) times range))
   (else '())))

(define (start-search-for-fast-prime start-time start range)
  (search-for-fast-primes start 1 range)
  (display (guile-time-of-day-difference (runtime) start-time)))

;; (start-search-for-fast-prime (runtime) 1000 3) ; => 42 microseconds
;; (start-search-for-fast-prime (runtime) 10000 3) ; => 85
;; (start-search-for-fast-prime (runtime) 100000 3) ; => 110
;; (start-search-for-fast-prime (runtime) 1000000 3) ; => 110
;; (start-search-for-fast-prime (runtime) 1000000000 3) ; => 270
;; (start-search-for-fast-prime (runtime) 1000000000000000 3) => 2045 microseconds

;; The order of growth is a little bit less than Theta log(n) for fast-prime.

;; Exercise 1.25

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;; (remainder  (fast-expt 3 1000000000000000000) 10000000)

;; (start-search-for-fast-prime (runtime) 1000000 3) => 2.7 seconds with fast expt

(remainder (fast-expt 3 1000000000) 1000000000) => hangs the vm
(expmod 3 100000000 100000000) => computes instantly

;; To be able to understand this huge difference in between Alyssa P. Hacker's suggestion as
;; compared to the author's version, we have to evaluate the process of the original expmod.
;; In expmod, the square function always tries to evaluate the remainder of the square of some value, which is always a small number. So everytime, square is evaluating a small number =>
(remainder (square (remainder (square (remainder (square 2) n)) n)) n)
;; In this version, the cpu does not have to square large values of numbers, hence it completes very quickly as compared to having to square large numbers in every recursion of Alyssa's version.

;; Exercise 1.26

;; In Scheme, the interpreter uses an applicative order evalution, which means that in a
;; procedure, the argument is evaluted first before being applied to the procedure.
;; In this case, it is square. Let's take a look at the square procedure:
(define (square x) (* x x))
;; Here, x will be evaluated once, then applied to *, so * works with two numbers.
;; In Louis Reasoner's case though, there is
(*
 (expmod base (/ exp 2) m)
 (expmod base (/ exp 2) m))

;; where the * procedure will evalute its arguments before being applied.
;; In this case (expmod base (/ exp 2) m) gets evaluted twice, where the same computation is being repeated to get a number. Contrary to square, it evalutes (expmod base (/ exp 2) m) once.
