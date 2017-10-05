(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;;; The partial-tree procedure used is unchanged from the book.
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))

(define (make-record key value)
  (list key value))

(define (key record)
  (car record))

(define records (list->tree (list
			     (make-record 1 'a)
			     (make-record 2 'b)
			     (make-record 3 'c)
			     (make-record 4 'd)
			     (make-record 5 'e))))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
	((= given-key (key (entry set-of-records)))
	 (entry set-of-records))
	((< given-key (key (entry set-of-records)))
	 (lookup given-key (left-branch set-of-records)))
	((> given-key (key (entry set-of-records)))
	 (lookup given-key (right-branch set-of-records)))))

(lookup 4 records)
;;; => (4 d)
