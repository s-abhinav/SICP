;;; a. Part 1
;;; The deriv procedure was further abstracted by replacing the sum? and
;;; product? dispatchers with get 'deriv. This defers the responsibility
;;; of evaluating the derivatives of the + or * operators to the installed
;;; package. The appropriate package is pulled depending on the evaluation
;;; of (get 'deriv (operator exp)). And then the operands, along with
;;; var, are applied to the package.

;;; a. Part 2
;;; The number? and same-variable? procedures cannot be used in the
;;; data-directed dispatch since they don't have the same number of arguments
;;; as the derivation procedures.

;;; b.
(define (install-sum-deriv-package)
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
	      (deriv (augend operands) var)))
  (put '(deriv) '+ deriv-sum))

(define (install-product-deriv-package)
  (define (deriv-product operands var)
    (make-sum
     (make-product (multiplier operands)
		   (deriv (multiplicand operands) var))
     (make-product (deriv (multiplier operands) var)
		   (multiplicand operands))))
  (put '(deriv) '* deriv-product))

;;; c.
(define (install-exponent-deriv-package)
  (define (deriv-exponent operands var)
    (make-product
     (make-product
      (exponent operands)
      (make-exponentiation (base operands) (make-sum (exponent operands) -1))
      )
     (deriv (base operands) var)))
  (put '(deriv) '** (deriv-exponent)))

;;; d.
;;; all the put procedure calls in every install package should be
;;; changed to (put 'operator '(deriv) (deriv-operator)).
