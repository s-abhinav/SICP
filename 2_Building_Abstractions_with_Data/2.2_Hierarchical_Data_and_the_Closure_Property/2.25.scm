(define first-list  '(1 3 (5 7) 9))
(define second-list '((7)))
(define third-list '(1 (2 (3 (4 (5 (6 7)))))))

(car (cdr (car (cdr (cdr first-list)))))

(car (car second-list))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr third-list))))))))))))

