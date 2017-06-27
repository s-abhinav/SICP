;;; Makes a rectangle using three coordinates.
;;; A rectangle can be represented with three coordinates only.
;;; The fourth can be inferred if needed and area and perimeter can be calculated
;;; with three coordinates.
(define (make-rect a b c)
  (cons a (cons b c)))

;;; Makes a rectangle using two segments.
;;; Only two lines are needed to represent a rectangle. The other two can be inferred.
;;; Area and perimeter calculations can be calculated with only two sides.
(define (make-rect-segment a b)
  (cons a b))

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

(define (square x)
  (* x x))

;;; Calculates the distance of a segment using the Pythagorean Theorem
(define (distance segment)
  (sqrt (+
	 (square
	  (-
	   (x-point (end-segment segment))
	   (x-point (start-segment segment))))
	 (square
	  (-
	   (y-point (end-segment segment))
	   (y-point (start-segment segment)))))))

;;; Get the coordinate of a vertex in a rectangle
;;; @arg position The position of a coordinate in a list of coordinates (1, 2, 3)
(define (get-rectangle-coordinate rectangle position)
  (define (iter coordinates n)
    (cond ((= n position)
	   (if (pair? (car coordinates))
	       (car coordinates)
	       coordinates))
	  (else
	   (iter (cdr coordinates) (+ n 1)))))
  (iter rectangle 1))

;;; Determines if the rectangle is structured using segments or coordinates.
;;; Returns true for segments, false for coordinates
(define (segment-rectangle? rectangle)
  (pair? (caar rectangle)))

;;; Get the horizontal line in segment of a rectangle irrespective of its structure
(define (horizontal-line rectangle)
  (if (segment-rectangle? rectangle)
      (car rectangle)
      (make-segment
       (get-rectangle-coordinate rectangle 1)
       (get-rectangle-coordinate rectangle 2))))

;;; Get the vertical line in segment of a rectangle irrespective of its structure
(define (vertical-line rectangle)
  (if (segment-rectangle? rectangle)
      (cdr rectangle)
      (make-segment
       (get-rectangle-coordinate rectangle 2)
       (get-rectangle-coordinate rectangle 3))))

;;; Calculates the perimeter of a rectangle
(define (perimeter rectangle)
  (* 2 (+ (distance (horizontal-line rectangle)) (distance (vertical-line rectangle)))))

;;; Calculates the area of a rectangle
(define (area rectangle)
  (* (distance (horizontal-line rectangle)) (distance (vertical-line rectangle))))

;;; Create a rectangle using lines
(define rectangle-segment
  (make-rect-segment (make-segment (make-point 0 0) (make-point 0 3))
		     (make-segment (make-point 0 3) (make-point 4 3)))) 

;;; Create a rectangle using coordinates
(define rectangle-coordinates (make-rect (make-point 0 0) (make-point 0 3) (make-point 4 3)))

(define area-rectangle-3-4 (* 3 4))
(define perimeter-rectangle-3-4 (* 2 (+ 3 4)))

;;; Test
(and
 (= area-rectangle-3-4 (area rectangle-segment))
 (= perimeter-rectangle-3-4 (perimeter rectangle-segment))
 (= area-rectangle-3-4 (area rectangle-coordinates))
 (= perimeter-rectangle-3-4 (perimeter rectangle-coordinates)))
