(define (tree->list tree)
  (if (null? tree)
      '()
      (append (tree->list (left-branch tree))
	      (cons (entry tree)
		    (tree->list (right-branch tree))))))

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

(define (union-set set1 set2)
  (list->tree (union-set-helper (tree->list set1) (tree->list set2) '())))

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

(union-set (list->tree '(1 2 3))
	   (list->tree '(4 5 6)))

;;; (3 (1 () (2 () ())) (5 (4 () ()) (6 () ()))) 

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((= (car set1) (car set2))
	 (cons (car set1) (intersection-set (cdr set1) (cdr set2))))
	((< (car set1) (car set2))
	 (intersection-set (cdr set1) set2))
	(else (intersection-set set1 (cdr set2)))))

(define (intersection-set-tree set1 set2)
  (list->tree (intersection-set (tree->list set1) (tree->list set2))))

(intersection-set-tree
 (list->tree '(1 2 3 5 7 8 9 10))
 (list->tree '(2 5 10 11)))

;;; (5 (2 () ()) (10 () ())) 
