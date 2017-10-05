(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))

;;; Partial tree works by splitting the list in three parts by first building
;;; sub trees on the left recursively. When this is complete, it does the same
;;; process with the right part of the tree's root, recursively processing
;;; the sub trees until it is complete. At the end, both the left sub tree
;;; and the right sub tree are consed with the middle part to make the root tree.
;;; Again, the consing of the trees happens recursively for each sub tree.

(list->tree (list 1 3 5 7 9 11))

;;; The Tree
;;;       5
;;;      /\
;;;     /  \
;;;    /    \
;;;   1      9
;;;    \    /\
;;;     3  7 11

;;; Since the procedure has to go through every element of the list,
;;; the order of growth would be O(n).




