(define nil '())

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (list new-row k))))

;;; Get the row number of a position in the cell.
(define (row-position position)
  (car position))

;;; Get the column number of a position in the cell.
(define (column-position position)
  (cadr position))

;;; Determine if the last-position is safe diagonally up with respect
;;; to the test-position.
(define (safe-diagonal-up? last-position test-position)
  (let ((column-difference
	 (- (column-position last-position) (column-position test-position))))
    (not (and
	  (= (- (row-position test-position) column-difference)
	     (row-position last-position))
	  (= (+ (column-position test-position) column-difference)
	     (column-position last-position))))))

;;; Determine if the last-position is safe diagonally down with respect
;;; to the test-position.
(define (safe-diagonal-down? last-position test-position)
  (let ((column-difference
	 (- (column-position last-position) (column-position test-position))))
    (not (and
	  (= (+ (row-position test-position) column-difference)
	     (row-position last-position))
	  (= (+ (column-position test-position) column-difference)
	     (column-position last-position))))))

;;; Determine if the last-position is horizontal with the test-position.
(define (safe-horizontal? last-position test-position)
  (let ((column-difference
	 (- (column-position last-position) (column-position test-position))))
    (not (and
	  (= (row-position test-position) (row-position last-position))
	  (= (+ (column-position test-position) column-difference)
	     (column-position last-position))))))

;;; Determine if the last position is safe with the previous positions.
(define (safe? k positions)
  (and-map (lambda (column)
	     (and (safe-diagonal-up?
		   (list-ref positions (- k 1))
		   (list-ref positions (- column 1)))

		  (safe-diagonal-down?
		   (list-ref positions (- k 1))
		   (list-ref positions (- column 1)))
		  
		  (safe-horizontal?
		   (list-ref positions (- k 1))
		   (list-ref positions (- column 1)))))
	   (enumerate-interval 1 (- k 1))))

;;; Define empty-board as an empty list 
(define empty-board '())

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

;; Test
(queens 4)
;; => (((2 1) (4 2) (1 3) (3 4)) ((3 1) (1 2) (4 3) (2 4)))
