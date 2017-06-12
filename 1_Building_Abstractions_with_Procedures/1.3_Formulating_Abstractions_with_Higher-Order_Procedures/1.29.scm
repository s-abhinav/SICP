(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (cube x)
  (* x x x))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (* (cond ((= k 0) 1)
	     ((= k n) 1)
	     ((odd? k) 4)
	     ((even? k) 2))
       (f (+ a (* k h)))))
  (*
   (/ h 3)
   (sum y 0 inc n)))

;; (simpsons-rule cube 0 1 1000)
;; 1/4
