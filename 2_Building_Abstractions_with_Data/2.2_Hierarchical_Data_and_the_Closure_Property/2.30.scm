(define nil '())

(define (square x)
  (* x x))

(define (square-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree)) (square-tree (cdr tree))))))


(define (square-tree-map tree)
  (map (lambda (subtree)
	 (cond ((not (pair? subtree)) (square subtree))
	       (else (square-tree-map subtree))))
       tree))

(define square-list  (list 1
			   (list 2 (list 3 4) 5)
			   (list 6 7)))

(square-tree square-list)

(square-tree-map square-list)
