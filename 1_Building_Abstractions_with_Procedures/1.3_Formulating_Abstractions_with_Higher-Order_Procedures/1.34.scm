(define (f g) (g 2))

;; When (f f) is evaluated, the interpreter evaluates (f 2)
;; g is replaced with f in the body of f
;; At this point, when (f 2) is evaluated, g is replaced with 2 so it becomes
;; (2 2)
;; The interpreter will complain that it either does not know of any procedure 2
;; which accepts an argument or will complain that 2 is not a procedure.
