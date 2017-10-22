;;; a.
(define (get-record personnel-file employee-name)
  ((get 'employee-record (division personnel-file)) employee-name))

;;; Each personnel file should have a division name and the employee-record
;;; operation procedure. This procedure should accept as argument, the name
;;; of the employee.

;;; b.
(define (get-salary employee)
  ((get 'salary employee)))

;;; The record should have an operation named salary, that when called,
;;; should return the value of the salary of the employee.
;;; Since (get 'salary employee) returns the salary procedure, it
;;; should be called so that it's evaluated by wrapping it in another
;;; pair of parentheses.

;;; c.
(define (find-employee-record personnel-file-list employee-name)
  (cond ((null? personnel-file-list) #f)
	(else
	 (let (
	       (result (get-record (car personnel-file-list) employee-name))
	       )
	   (if (not (null? result))
	       result
	       (find-employee-record
		(cdr personnel-file-list employee-name)))))))

;;; d.
;;; A new division package must be installed into the system.


