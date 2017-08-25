(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;; Build a list of distinct positive integers from 1 to n.
(define (ordered-triples n)
  (flatmap (lambda (i)
	     (flatmap 
	      (lambda (j)
		(map (lambda (k)
		       (list i j k))
		     (enumerate-interval 1 (- j 1))))
	      (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

;; Calculate the sum of the numbers in a list.
(define (sum list)
  (accumulate + 0 list))

(define (ordered-triples-sum n s)
  (filter (lambda (triple-sum)
	    (= (cdr triple-sum) s))
	  (map (lambda (triple-sum-pair)
		 (cons triple-sum-pair (sum triple-sum-pair)))
	       (ordered-triples n))))

(ordered-triples-sum 6 10)
;; => (((5 3 2) . 10) ((5 4 1) . 10) ((6 3 1) . 10))
