(define (sum-squares a b)
  (+ (* a a) (* b b)))

(define (sum-squares-two-larger a b c)
  (cond ((and (< c a) (< c b))
	 (sum-squares a b))
	((and (< b a) (< b c))
	 (sum-squares a c))
	((and (< a b) (< a c))
	 (sum-squares b c))))
