(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; The procedure that will be applied to the two parameters is conditional upon the value
;; of one of the parameters. The procedure will be unknown until at evaluation time
;; of the arguments.
;; In the case of a-plus-abs-b, the procedure + or - will be determined based on the
;; value of b. If b is greater than 0, then the procedure + will be applied to both a and b.
;; Else, the procedure - will be applied to a and b.
