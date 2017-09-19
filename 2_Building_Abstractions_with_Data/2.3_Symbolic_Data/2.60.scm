;;; Guile does not have a true / false definition
(define true #t)
(define false #f)

;;; This remains the same.
;;; Order of growth is O(n).
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

;;; There is no need to do any verification before adjoining.
;;; O(1) as opposed to O(n).
(define (adjoin-set x set)
  (cons x set))

;;; Since this version allows for duplicates, there is no need
;;; to verify if an element in set1 is in set2.
;;; O(1) as opposed to O(n²)
(define (union-set set1 set2)
  (append set1 set2))

;;; The intersection procedure also remains the same.
;;; O(n²)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

(define set1 '(1 2 3 4))
(define set2 '(2 3 5 6))

(intersection-set set1 set2)
;;; => (2 3)

(union-set set1 set2)
;;; => (4 3 2 1 2 3 5 6)

;;; This could be used in bit transmission. A set of repeated 0s and 1s represent something.
;;; Stripping off the repeated values will destroy the information.
