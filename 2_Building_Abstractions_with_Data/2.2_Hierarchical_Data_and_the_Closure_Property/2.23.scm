(define (for-each f values)
  (cond ((not (null? values))
	 (f (car values))
	 (for-each f (cdr values)))))

(for-each 
 (lambda (x) (newline) (display x))
 (list 57 321 88))

;; => 
;; 57
;; 321
;; 88
