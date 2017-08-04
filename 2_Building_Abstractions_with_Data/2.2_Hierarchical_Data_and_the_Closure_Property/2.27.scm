(define x (list (list 1 2) (list 3 4)))
(define y '(1 2 (3 4) (5 (6 7))))

(define (reverse alist)
  (define (iter alist result)
    (cond ((null? alist) result)
	  (else (iter (cdr alist) (cons (car alist) result)))))
  (iter alist '()))

(define (deep-reverse alist)
  (define (iter-reverse alist result)
    (cond ((null? alist) result)
	  ((list? (car alist))
	   (iter-reverse (cdr alist) (cons (deep-reverse (car alist)) result)))
	  (else (iter-reverse (cdr alist) (cons (car alist) result)))))
  (iter-reverse alist '()))

(reverse x)
;; => ((3 4) (1 2))

(reverse y)
;; => ((5 (6 7)) (3 4) 2 1)

(deep-reverse x)
;; => ((4 3) (2 1))

(deep-reverse y)
;; => (((7 6) 5) (4 3) 2 1)
