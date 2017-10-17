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

;;; Exercise 2.69
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)    ; symbol
			       (cadr pair))  ; frequency
		    (make-leaf-set (cdr pairs))))))

;;; From Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (fold-left make-code-tree (car leaf-set) (cdr leaf-set)))

(define generated-tree (generate-huffman-tree '((A 8) (B 2) (D 1) (C 1))))

(decode (encode '(A D A B B C A) generated-tree) generated-tree)

;;; => (A D A B B C A)

;;; Exercise 2.70
(define 1950-lyrics-huffman-tree
 (generate-huffman-tree '(
			  (A 2)
			  (BOOM 1)
			  (GET 2)
			  (JOB 2)
			  (NA 16)
			  (SHA 3)
			  (YIP 9)
			  (WAH 1))))

(define lyrics '(
		 GET A JOB
		     SHA NA NA NA NA NA NA NA NA
		     GET A JOB
		     SHA NA NA NA NA NA NA NA NA
		     WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
		     SHA BOOM
		     ))

(length (encode lyrics 1950-lyrics-huffman-tree))

;;; => 87

#|
Since there are 8 symbols, the number of bits required for encoding is defined
by 2^n = 8 where n is 3.
The number of symbols in the song is 36.
Therefore, the number of bits required for the fixed-length code to encode this
song would be 36 * 3 which is 108.
|#

;;; Exercise 2.71

(define 5-symbols-huffman-tree
  (generate-huffman-tree
   '(
     (A 1)
     (B 2)
     (C 4)
     (D 8)
     (E 16))))

;;; => (((((leaf A 1) (leaf B 2) (A B) 3) (leaf C 4) (A B C) 7) (leaf D 8) (A B C D) 15) (leaf E 16) (A B C D E) 31)

#|

                           {A B C D E} 31
                                 *
                                / \
                               /   \
                 {A B C D} 15 *    E 16
                     ________/ \________
                    /                   \
         {A B C} 7 *                     D 8
            ______/ \_____
          /               \
 {A B} 3 *                 C 4
        / \
       /   \
     A 1   B 2        

The most frequent symbol requires 1 bit and the least frequent symbol requires
n - 1 bits.

The tree for size 10 will have the same pattern as 5.
|#

#|
Exercise 2.72
Part 1.
1. in-set? uses O(n).
2. At worst, the encode-symbol procedure should iterate 3 times to encode a symbol, which
   is O(n-1) which is almost O(n).
The order of growth is O(n²).

Part 2.
For the most frequent symbol, encode-symbol would call in-left?, and if nothing is found, would then call in-right?. At worst, this would take O(n) and at best, O(1).
encode-symbol itself would need to run once, without any iteration, since the most frequent symbol is found in the top branch. This takes O(1). At best, it would take O(1) and at worst, O(n) to encode the symbol.

For the least frequent symbol, in-set? would take O(n) to look for the symbol and encode-symbol would also take O(n) to construct the bits. The order of growth would hence be O(n²).
|#
