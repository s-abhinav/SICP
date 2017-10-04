(define (union-set set1 set2)
  (union-set-helper set1 set2 '()))

(define (union-set-helper set1 set2 result)
  (cond
   ((null? set1) (append result set2))
   ((null? set2) (append result set1))
   ((= (car set1) (car set2))
    (union-set-helper (cdr set1) (cdr set2) (append result (list (car set1)))))
   ((< (car set1) (car set2))
    (union-set-helper (cdr set1) set2 (append result (list (car set1)))))
   ((< (car set2) (car set1))
    (union-set-helper set1 (cdr set2) (append result (list (car set2)))))))

(union-set '(2 5 9) '(1 4 6 8))
