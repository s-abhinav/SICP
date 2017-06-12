(define (double a)
  (lambda (x)
    (a (a x))))

(define (inc n)
  (+ n 1))

;; (((double double) inc) 0)
;; This applies double to double, which means that it applies twice inc to twice inc
;; which is 4 times inc
;; => 4
;; This is how it works
;; The argument double has two nodes. The two nodes are then applied to themselves where
;; every node gets two more nodes, which makes it four.

;; (((double (double double)) inc) 0)
;; This applies double to double, which is 4 and then applies 4 nodes to every node which is equivalent to 4 * 4. Because double applies the argument twice, double is applying itself twice where the result is applied to itself. This is like recurse one more time.

;; (((double (double double)) inc) 5)
;; => Here, 5 will be incremented 16 times
