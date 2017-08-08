(define x (list (list 1 2) (list 3 4)))

(define y '(1 2 (3 4 (5 6 (7 8)))))

(define (fringe x)
  (cond 
   ((null? x) '())
   ((pair? (car x)) (append (fringe (car x)) (fringe (cdr x))))
   (else (append (list (car x)) (fringe (cdr x))))))

(fringe x)
;; => (1 2 3 4)

(fringe y)
;; => (1 2 3 4 5 6 7 8)
