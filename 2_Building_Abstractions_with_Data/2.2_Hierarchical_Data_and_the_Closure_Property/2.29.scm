(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; d.
;; (define (make-mobile left right)
;;   (cons left right))

;; (define (make-branch length structure)
;;   (cons length structure))

(define (branch? x)
  (and (pair? x) (not (pair? (car x)))))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

;; d.
;; (define (right-branch mobile)
;;   (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cond ((not (pair? branch)) branch)
	(else
	 (car (cdr branch)))))

;; d.
;; (define (branch-structure branch)
;;   (cond ((not (pair? branch)) branch)
;; 	(else
;; 	 (cdr branch))))


(define (total-weight mobile)
  (let ((left-branch-structure (branch-structure (left-branch mobile)))
	(right-branch-structure (branch-structure (right-branch mobile))))
    (cond ((null? mobile) 0)
	  (else
	   (+
	    (if (pair? left-branch-structure)
		(total-weight left-branch-structure)
		left-branch-structure)
	    (if (pair? right-branch-structure)
		(total-weight right-branch-structure)
		right-branch-structure))))))

;; Total weight: 3
(define mobile-left (make-mobile (make-branch 10 1) (make-branch 10 2)))

;; Total weight: 7
(define mobile-right (make-mobile (make-branch 10 3) (make-branch 10 4)))

;; Use mobile-left and mobile-right as branches
(define mobile-center
  (make-mobile (make-branch 20 mobile-left) (make-branch 20 mobile-right)))

;; Use mobile-center and mobile-left as branches
(define mobile-center-left-mobile
  (make-mobile (make-branch 30 mobile-left) (make-branch 30 mobile-center)))

;; Use mobile-center and create a weight to the left
(define mobile-center-left-weight
  (make-mobile (make-branch 40 2) (make-branch 30 mobile-center)))

;; Total Weight Tests
(define total-weight-tests
  (and (= 3 (total-weight mobile-left))
       (= 7 (total-weight mobile-right))
       (= 10 (total-weight mobile-center))
       (= (+ 10 3) (total-weight mobile-center-left-mobile))
       (= (+ 10 2) (total-weight mobile-center-left-weight))))

(define (torque branch)
  (cond
   ((branch? branch)
    (* (branch-length branch)
       (if (pair? (branch-structure branch))
	   (total-weight (branch-structure branch))
	   (branch-structure branch))))
   (else 0)))

(define (balanced-mobile? mobile)
  (cond ((null? mobile) #t)
	((not (pair? mobile)) #t)
	(else (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
		   (and (eq? #t (balanced-mobile? (left-branch mobile)))
			(eq? #t (balanced-mobile? (right-branch mobile))))))))

(define balanced-mobile-1 (make-mobile (make-branch 2 3) (make-branch 3 2)))
(define balanced-mobile-2 (make-mobile (make-branch 3 3) (make-branch 3 3)))

(define unbanlanced (make-mobile
		     (make-branch 1 balanced-mobile-1)
		     (make-branch 1 balanced-mobile-2)))

(define twice-balanced-mobile-1 (make-mobile
				 (make-branch 2 balanced-mobile-1)
				 (make-branch 2 balanced-mobile-1)))

;; Balanced Mobile Tests
(define balanced-mobile-tests
  (and (eq? #t (balanced-mobile? balanced-mobile-1))
       (eq? #t (balanced-mobile? balanced-mobile-2))
       (eq? #t (balanced-mobile? twice-balanced-mobile-1))
       (eq? #f (balanced-mobile? unbanlanced))))

(and
 total-weight-tests
 balanced-mobile-tests)

;; d.
;; Only the way that the second value in the structure gets accessed needs
;; to be changed. In this case, for the list, to access the second value
;; we need to do a (car (cdr value)) since the second element is a list.
;; For a cons, the second value is not necessarily a list. Hence,
;; a (car (cdr value) is not needed, but only a (cdr value).
