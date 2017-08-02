(define (last-pair alist)
  (cond ((null? (cdr alist)) alist)
	(else (last-pair (cdr alist)))))

(last-pair (list 23 72 149 34))
