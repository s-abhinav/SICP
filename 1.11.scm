(use-modules (statprof)) ; statistical profiler for Guile

(define (f n)
  (if (< n 3)
      n
      (+
       (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(define (f-iter-helper n)
  (f-iter 0 1 2 n))

(define (f-iter f0 f1 f2 n)
  (cond ((< n 3)
	 n)
	((= n 3)
	 (+ f2 (* 2 f1) (* 3 f0)))
	(else
	 (f-iter f1 f2 (+ f2 (* 2 f1) (* 3 f0)) (- n 1)))))

;; Test case
(= (f 31) (f-iter-helper 31))

;; Run the statistical profiler for the recursive process.
(statprof (lambda ()
	    (f 31)
	    #f))

;; %     cumulative   self             
;; time   seconds     seconds      name
;; 100.00     11.19     11.19  f
;;   0.00     22.37      0.00  #<procedure 804d6de40 ()>
;;   0.00     11.19      0.00  #<procedure 8036f9440 at ice-9/top-repl.scm:66:5 ()>
;;   0.00     11.19      0.00  call-with-prompt
;;   0.00     11.19      0.00  catch
;;   0.00     11.19      0.00  ev
;;   0.00     11.19      0.00  #<procedure 804628640 at <current input>:399:26 ()>
;;   0.00     11.19      0.00  statprof
;;   0.00     11.19      0.00  with-error-to-port
;;   0.00     11.19      0.00  call-with-result
;;   0.00     11.19      0.00  #<procedure 80447a2a0 at statprof.scm:655:4 ()>
;;   0.00     11.19      0.00  #<procedure 804d6dd50 at geiser/evaluation.scm:80:21 ()>
;;   0.00     11.19      0.00  #<procedure 8036f9540 at ice-9/top-repl.scm:31:6 (thunk)>
;;   0.00     11.19      0.00  start-repl*
;;   0.00     11.19      0.00  call-with-output-string
;;   0.00     11.19      0.00  apply-smob/1
;;   0.00     11.19      0.00  run-repl*
;;   0.00     11.19      0.00  %start-stack
;;   0.00     11.19      0.00  with-default-trap-handler
;;   0.00     11.19      0.00  #<procedure 804d6dde0 at geiser/evaluation.scm:76:15 ()>
;;   0.00     11.19      0.00  with-output-to-port
;;   0.00     11.19      0.00  eval
;; ---
;; Sample count: 1055
;; Total time: 11.186796 seconds (0.0 seconds in GC)

;; Run the statistical profiler for the iterative process.
(statprof (lambda ()
	    (f-iter-helper 100000)
	    #f))

;; %     cumulative   self             
;; time   seconds     seconds      name
;;  77.62      1.66      1.66  %run-finalizers
;;  22.38      2.14      0.48  f-iter
;;   0.00      4.28      0.00  #<procedure 804d6d9c0 ()>
;;   0.00      2.14      0.00  #<procedure 8036f9440 at ice-9/top-repl.scm:66:5 ()>
;;   0.00      2.14      0.00  call-with-prompt
;;   0.00      2.14      0.00  catch
;;   0.00      2.14      0.00  ev
;;   0.00      2.14      0.00  statprof
;;   0.00      2.14      0.00  with-error-to-port
;;   0.00      2.14      0.00  call-with-result
;;   0.00      2.14      0.00  #<procedure 804d6d8d0 at statprof.scm:655:4 ()>
;;   0.00      2.14      0.00  #<procedure 8043bb030 at geiser/evaluation.scm:80:21 ()>
;;   0.00      2.14      0.00  #<procedure 8036f9540 at ice-9/top-repl.scm:31:6 (thunk)>
;;   0.00      2.14      0.00  start-repl*
;;   0.00      2.14      0.00  call-with-output-string
;;   0.00      2.14      0.00  apply-smob/1
;;   0.00      2.14      0.00  run-repl*
;;   0.00      2.14      0.00  %start-stack
;;   0.00      2.14      0.00  with-default-trap-handler
;;   0.00      2.14      0.00  #<procedure 803f8a440 at <current input>:385:26 ()>
;;   0.00      2.14      0.00  #<procedure 8043bb0c0 at geiser/evaluation.scm:76:15 ()>
;;   0.00      2.14      0.00  with-output-to-port
;;   0.00      2.14      0.00  eval
;; ---
;; Sample count: 143
;; Total time: 2.138555 seconds (1.407848 seconds in GC)
