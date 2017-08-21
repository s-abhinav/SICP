(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate
   (lambda (x y)
     (+ x y))
   0
   (map (lambda (a)
	  (cond ((null? a) 0)
		((pair? a) (count-leaves a))
		(else 1)))
	t)))

;; The count-leaves procedure of section 2-2-2
(define (count-leaves-2-2-2 x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))

;; Tests

(and
 (= (count-leaves-2-2-2 x) (count-leaves x))
 (= (count-leaves-2-2-2 (list x x)) (count-leaves (list x x))))
