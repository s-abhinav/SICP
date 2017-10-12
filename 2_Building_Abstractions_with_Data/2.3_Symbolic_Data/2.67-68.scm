(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))


(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

sample-message

(decode sample-message sample-tree)
;;; => (A D A B B C A)

;;; Exercise 2.68
(define (in-set? x set)
  (cond ((null? set) #f)
	((eq? x (car set)) #t)
	(else (in-set? x (cdr set)))))

(define (in-left? x tree)
  (cond ((null? tree) '())
	((in-set? x (symbols (left-branch tree))) #t)
	(else #f)))

(define (in-right? x tree)
  (cond ((null? tree) '())
	((in-set? x (symbols (right-branch tree))) #t)
	(else #f)))

(define (encode-symbol symbol tree)
  (cond ((null? tree) '())
	((leaf? tree) '())
	((in-left? symbol tree) (cons '0 (encode-symbol symbol (left-branch tree))))
	((in-right? symbol tree) (cons '1 (encode-symbol symbol (right-branch tree))))
	(else (error symbol "is not in the tree"))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(decode (encode '(A D A B B C A) sample-tree) sample-tree)
;;; => (A D A B B C A)
