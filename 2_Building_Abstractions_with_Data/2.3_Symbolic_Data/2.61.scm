(define (adjoin-set x set)
  (adjoin-set-helper x '() set))

(define (adjoin-set-helper x left right)
  (cond	((null? right) (append left (list x)))
	((= x (car right)) (append left right))
	((< x (car right)) (append left (list x) right))
	(else (adjoin-set-helper x (append left (list (car right))) (cdr right)))))

;;; Add in the middle.
(adjoin-set '4 '(1 2 3 5))
;;; => (1 2 3 4 5)

;;; Prepend
(adjoin-set '1 '(2 3 4 5))
;;; => (1 2 3 4 5)

;;; Append
(adjoin-set '5 '(1 2 3 4))
;;; => (1 2 3 4 5)

;;; Ignore duplicates
(adjoin-set '2 '(1 2 3))
;;; (1 2 3)
