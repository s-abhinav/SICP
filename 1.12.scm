;; Compute the number of Pascal's triangle at position x y where x is the row number and y is the column number, starting at 0 for the first row or column.
;; Example: to compute the value at row 5, column 3
;; (f 4 2) => 6
;; (f 4 3) => 4
(define (f x y)
  (if (or (= y 0) (= y x))
      1
      (+
       (f (- x 1)
	  (- y 1))
       (f (- x 1) y))))
