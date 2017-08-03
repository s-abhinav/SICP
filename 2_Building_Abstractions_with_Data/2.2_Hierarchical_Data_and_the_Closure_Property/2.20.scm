(define (parity value)
  (cond ((even? value) even?)
	(else odd?)))

(define (same-parity x . y)
  (define (same-parity-helper y)
    (cond ((null? y) '())
	  (((parity x) (car y)) (cons (car y) (same-parity-helper (cdr y))))
	  (else (same-parity-helper (cdr y)))))
  (cons x (same-parity-helper y)))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)
