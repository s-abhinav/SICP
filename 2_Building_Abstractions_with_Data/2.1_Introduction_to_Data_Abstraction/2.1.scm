(define (make-rat n d)
  (if (and (> n 0) (< d 0))
      (cons (* n -1) (* d -1))
      (cons n d)))

(make-rat 1 2)
;; => (1 . 2)

(make-rat -1 2)
;; => (-1 . 2)

(make-rat 1 -2)
;; => (-1 . 2)

(make-rat -1 -2)
;; => (-1 . -2)
