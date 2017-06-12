; Example multiplication procedure
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double a)
  (+ a a))

;; Define halve in terms of addition
(define (halve-iter a b c)
  (if (or (= a b) (= a (+ b 1)))
      c
      (halve-iter a (+ b 2) (+ c 1))))

(define (halve a)
  (halve-iter a 0 0))

;; Exercise 1.17
;; Fast multiply recursive
(define (fast-mul-r a b)
  (cond ((= b 0)
	 0)
	((even? b)
	 (double (fast-mul-r a (halve b))))
	(else (+ a (fast-mul-r a (- b 1))))))

;; Test Cases
;;;; Recursive
(and 
 (= (* 2 5) (fast-mul-r 2 5))
 (= (* 3 0) (fast-mul-r 3 0))
 (= (* 0 3) (fast-mul-r 0 3))
 (= (* 10 2) (fast-mul-r 10 2))
 (= (* 10 3) (fast-mul-r 10 3))
 (= (* 7 2) (fast-mul-r 7 2))
 (= (* 7 3) (fast-mul-r 7 3)))

;; Exercise 1.18
;; Fast multiply iterative
(define (fast-mul-iter a b c)
  (cond
   ((= b 0)
    0)
   ((= b 1)
    (+ a c))
   ((even? b)
    (fast-mul-iter (double a) (halve b) c))
   (else
    (fast-mul-iter a (- b 1) (+ a c)))))

;; Fast multiply helper
(define (fast-mul a b)
  (fast-mul-iter a b 0))

;; Test cases
;;;; Iterative
(and
 (= (* 2 5) (fast-mul 2 5))
 (= (* 3 0) (fast-mul 3 0))
 (= (* 0 3) (fast-mul 0 3))
 (= (* 10 2) (fast-mul 10 2))
 (= (* 10 3) (fast-mul 10 3))
 (= (* 7 2) (fast-mul 7 2))
 (= (* 7 3) (fast-mul 7 3)))
