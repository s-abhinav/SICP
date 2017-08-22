(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init	(map (lambda (a)
				       (car a)) seqs))
	    (accumulate-n op init (map (lambda (a)
					 (cdr a)) seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product '(3 4) '(1 2))
;; => 11

(define (matrix-*-vector m v)
  (map (lambda (x)
	 (dot-product x v)) m))

(matrix-*-vector '((3 4) (5 6)) '(1 2))
;; => (11 17)

(define (transpose mat)
  (accumulate-n cons nil mat))

(transpose '((1 2 3)
	     (4 5 6)))
;; =>
;; ((1 4)
;;  (2 5)
;;  (3 6))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (a)
	   (matrix-*-vector cols a)) m)))

(matrix-*-matrix '((3 4)
		   (5 6))

		 '((1 2)
		   (7 8)))

;; =>
;; ((31 38)
;;  (47 58))
