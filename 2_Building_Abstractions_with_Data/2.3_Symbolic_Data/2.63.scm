;;; a. Both procedures produce the same result. A flat ordered list.
;;; b. Both procedures have the same order of growth, since they both have to
;;; process the same number of nodes.

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

;;; Trees from Figure 2.16

(define first-figure
  (make-tree 7
	     (make-tree 3
			(make-tree 1 '() '())
			(make-tree 5 '() '()))
	     (make-tree 9
			'()
			(make-tree 11 '() '()))))

(define second-figure
  (make-tree 3
	     (make-tree 1 '() '())
	     (make-tree 7
			(make-tree 5 '() '())
			(make-tree 9
				   '()
				   (make-tree 11 '() '())))))

(define third-figure
  (make-tree 5
	     (make-tree 3
			(make-tree 1 '() '())
			'())
	     (make-tree 9
			(make-tree 7 '() '())
			(make-tree 11 '() '()))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

