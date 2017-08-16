(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y)
		(cons (p x) y))
	      nil sequence))

(define (square x)
  (* x x))

(map square '(1 2 3))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append '(1 2 3) '(4 5 6))
(append '((1 2 (3))) '(7 (8) 9))

(define (length sequence)
  (accumulate (lambda (x y)
		(+ 1 y)) 0 sequence))

(length '(a b c d e f))
;; => 6

(length '(ab (cd (xy)) ef))
;; => 3
