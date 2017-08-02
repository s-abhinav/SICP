(define (reverse alist)
  (define (iter alist result)
    (cond ((null? alist) result)
	  (else (iter (cdr alist) (cons (car alist) result)))))
(iter alist '()))

(reverse (list 1 4 9 16 25))
