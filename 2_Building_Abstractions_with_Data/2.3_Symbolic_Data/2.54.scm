(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
	((not (pair? a)) (eq? a b))
	(else
	 (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b))))))

;;; Tests
(and
 (eq? #t (equal? '(this is a list) '(this is a list)))
 (eq? #f (equal? '(this is a list) '(this (is a) list)))
 (eq? #t (equal? '() '()))
 (eq? #t (equal? "quote" "quote"))
 (eq? #f (equal? "quote" "list")))
