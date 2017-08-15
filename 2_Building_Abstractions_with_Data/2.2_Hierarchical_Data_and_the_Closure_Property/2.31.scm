(define (tree-map f tree)
  (map (lambda (subtree)
	 (cond ((not (pair? subtree)) (f subtree))
	       (else (tree-map f subtree))))
       tree))

(define (square-tree tree) (tree-map square tree))

(define square-list  (list 1
			   (list 2 (list 3 4) 5)
			   (list 6 7)))

(square-tree square-list)
