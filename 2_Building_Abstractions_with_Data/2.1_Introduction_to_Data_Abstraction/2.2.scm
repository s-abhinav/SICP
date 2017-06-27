(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (make-point (+ (x-point (start-segment segment))
		 (/ (- (x-point (end-segment segment))
		       (x-point (start-segment segment))) 2))

	      (+ (y-point (start-segment segment))
		 (/ (- (y-point (end-segment segment))
		       (y-point (start-segment segment))) 2))
	      ))

(define segment1 (make-segment (make-point 2 2) (make-point 4 4)))
(midpoint-segment segment1)
;; => (3 . 3)

(define segment2 (make-segment (make-point -10 -6) (make-point 2 4)))
(midpoint-segment segment2)
;; => (-4 . -1)
