(define (cons a b)
  (*
   (expt 2 a)
   (expt 3 b)))

(define (car z)
  (define (iter y n)
    (cond ((odd? y) n)
	  (else (iter (/ y 2) (+ 1 n)))))
  (iter z 0))

(define (cdr z)
  (define (iter y n)
    (cond ((= 0 (modulo y 3))
	   (iter (/ y 3) (+ 1 n)))
	  (else n)))
  (iter z 0))

;; Tests
(and
 (= 5 (car (cons 5 7)))
 (= 7 (cdr (cons 5 7)))
 (= 11 (car (cons 11 (cons 5 7))))
 (= 13 (cdr (cons (cons 5 7) 13)))
 (= 1 (car (car (cons (cons 1 2) 3))))
 (= 2 (cdr (car (cons (cons 1 2) 3))))
 (= 5 (car (cdr (cons 11 (cons 5 7)))))
 (= 7 (cdr (cdr (cons 11 (cons 5 7)))))
 (= 2 (car (car (cons (cons 2 3) (cons 4 5)))))
 (= 3 (cdr (car (cons (cons 2 3) (cons 4 5)))))
 (= 4 (car (cdr (cons (cons 2 3) (cons 4 5)))))
 (= 5 (cdr (cdr (cons (cons 2 3) (cons 4 5)))))
 )
