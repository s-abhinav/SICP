(define (inc n)
  (+ n 1))
 
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define zero
  (lambda (f) (lambda (x) x)))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define three
  (lambda (f) (lambda (x) (f (f (f x))))))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define (print-number n)
  ((n inc) 0))

;;; Tests
(and
 (= 0 (print-number zero))
 (= 1 (print-number one))
 (= 2 (print-number two))
 (= 3 (print-number three))
 (= 5 (print-number (add three two)))
 (= 0 (print-number (add zero zero)))
 (= 1 (print-number (add zero one)))
 (= 6 (print-number (add three three))))
