(define (square x)
  (* x x))

(define nil '())

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(square-list (list 1 2 3 4))

;; => (16 9 4 1)

;; The answer list gets constructed in a reverse order since cons
;; always pushes a new value on top answer.
;; Initially, answer will be nil. When the first value gets consed,
;; it becomes (1). On the second iteration, answer contains (1)
;; and cons will add 4 to it, so it will become (4 1).

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square 
                     (car things))))))
  (iter items nil))

(square-list (list 1 2 3 4))

;; => ((((() . 1) . 4) . 9) . 16)

;; The answer got constructed in an unexpected structure. Louis was hoping
;; that the structure would be a list, but instead, nested pairs got constructed.
;; This happened because on every iteration, a list is consed with an atom.
;; To be able to get a flat list structure, cons expects the tail to be a list and
;; and the head to be an atom.
