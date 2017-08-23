(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
;; => 3/2

(fold-left / 1 (list 1 2 3))
;; => 1/6

(fold-right list nil (list 1 2 3))
;; => (1 (2 (3 ())))

(fold-left list nil (list 1 2 3))
;; => (((() 1) 2) 3)

;; In order for fold-right and fold-left to produce the same value,
;; op should return the same value irrespective of the operand position.
;; In other words, the operand values can be interchanged for op and
;; op will still give the same results. Examples are + and *.
