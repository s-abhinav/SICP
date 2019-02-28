(define true #t)

(define false #f)

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

(define (amb? exp) (tagged-list? exp 'amb))

(define (amb-choices exp) (cdr exp))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-predicate clause) (car clause))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-actions clause) (cdr clause))

(define (make-begin seq) (cons 'begin seq))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no `else' clause
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF"
		       clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (application? exp)
  (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
		   (cddr exp)))) ; body

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (assignment-value exp) (caddr exp))

(define (assignment-variable exp) (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (text-of-quotation exp) (cadr exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (variable? exp)
  (symbol? exp))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))

;;; Operations on Environments

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((and (eq? var (car vars))
		  (eq? (car vals) '*unassigned*))
	     (error "Unassigned variable" var))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)

(define (eval-sequence exps env)
  (debug "eval-sequence:" exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
	(else (eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

(define (make-procedure parameters body env)
  (debug "make-procedure: " body env)
  (debug "scan-out-defines: " (scan-out-defines body) env)
  (list 'procedure parameters (scan-out-defines body) env)
  ; (list 'procedure parameters body env)
  )

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))


;;; Evaluator

(define (debug type exp env)
  ;; (display type) (display exp)
  ;;  (newline)
  ;; (display "env:") (display env)
  ;; (newline)
  '()
  )

(define (eval exp env)
  ((analyze exp) env))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond
   ((amb? exp) (analyze-amb exp))
   ((self-evaluating? exp)
    (analyze-self-evaluating exp))
   ((let? exp)
    (analyze (let->combination exp)))
   ((quoted? exp) (analyze-quoted exp))
   ((variable? exp) (analyze-variable exp))
   ((assignment? exp) (analyze-assignment exp))
   ((definition? exp) (analyze-definition exp))
   ((if? exp) (analyze-if exp))
   ((lambda? exp) (analyze-lambda exp))
   ((begin? exp) (analyze-sequence (begin-actions exp)))
   ((cond? exp) (analyze (cond->if exp)))
   ((application? exp) (analyze-application exp))
   (else
    (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain `pred-value'
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling `a'
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling `a'
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
       ;; success continuation for this `aproc'
       (lambda (arg fail2)
         (get-args (cdr aprocs)
                   env
                   ;; success continuation for recursive
                   ;; call to `get-args'
                   (lambda (args fail3)
                     (succeed (cons arg args)
                              fail3))
                   fail2))
       fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

(define amb-counter 0)

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
	(set! amb-counter (+ 1 amb-counter))
        (if (null? choices)
            (fail)
            ((car choices) env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))

;;; Do not re evaluate this procedure twice in the
;;; same session. Doing this twice breaks the interpreter
(define apply-in-underlying-scheme apply)

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (if (procedure? proc)
      proc
      (cadr proc)))

(define (apply-primitive-procedure proc args)
  (if (eq? 1 (procedure-minimum-arity proc))
      (proc args)
      (apply-in-underlying-scheme
       (primitive-implementation proc) args)))

(define (apply procedure arguments)
  (cond ((or (procedure? procedure) (primitive-procedure? procedure))
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (newline)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(else
	 (error
	  "Unknown procedure type -- APPLY" procedure))))

;;; Running the Evaluator

(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'list list)
	(list 'null? null?)
	(list '= =)
	(list '> >)
	(list '< <)
	(list '* *)
	(list '+ +)
	(list '- -)
	(list 'not not)
	(list 'eq? eq?)
	(list 'remainder remainder)
	(list 'abs abs)
	(list 'member member)))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

;;; let combinations

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-expression-definitions exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(define (let-vars expression-definitions)
  (cond ((null? expression-definitions) '())
	(else (cons (caar expression-definitions)
	       (let-vars (cdr expression-definitions))))))

(define (let-exprs expression-definitions)
  (cond ((null? expression-definitions) '())
	(else (cons (cadar expression-definitions)
	       (let-exprs (cdr expression-definitions))))))

(define (let->combination exp)
  (cons
   (cons
    'lambda
    (cons
     (let-vars (let-expression-definitions exp))
     (let-body exp)))
   (let-exprs (let-expression-definitions exp))))


;;; letrec combinations

(define (letrec? exp)
  (tagged-list? exp 'letrec))

(define (letrec->let exp)
  (cons 'let
	(append (list (letrec-variable-unassigns exp))
		(letrec-variable-sets exp)
		(letrec-body exp))))

(define (letrec-variables exp)
  (map (lambda (mapping) (car mapping)) (cadr exp)))

(define (letrec-expression-definitions exp)
  (map (lambda (mapping) (cadr mapping)) (cadr exp)))

(define (letrec-variable-unassigns exp)
  (map (lambda (variable)
	 (list variable ''*unassignment*))
       (letrec-variables exp)))

(define (letrec-variable-sets exp)
  (map (lambda (variable expression)
	 (list 'set! variable expression))
       (letrec-variables exp)
       (letrec-expression-definitions exp)))

(define (letrec-body exp) (cddr exp))


;;; scan out defines

(define (scan-out-defines p-body)
  (if (and
       (not (procedure? p-body))
       (not (null? (procedure-variable-body-mapping p-body))))
      (list
       (let->combination
	(append
	 (list
	  'let
	  (procedure-defines-unassigned
	   (procedure-defines
	    (procedure-variable-body-mapping p-body))))
	 (append
	  (procedure-assignment
	   (procedure-variable-body-mapping p-body))
	  (procedure-expressions p-body)))))
      p-body))

(define (procedure-expressions p-body)
  (cond ((null? p-body) '())
	((definition? (car p-body))
	 (procedure-expressions (cdr p-body)))
	(else p-body)))

(define (procedure-variable-body-mapping p-body)
  (cond ((null? p-body) '())
	((definition? (car p-body))
	 (cons (list
		(definition-variable (car p-body))
		(definition-value (car p-body)))
	       (procedure-variable-body-mapping (cdr p-body))))
	(else '())))

(define (procedure-defines mapping)
  (cond ((null? mapping) '())
	(else (cons
	       (caar mapping)
	       (procedure-defines (cdr mapping))))))

(define (procedure-defines-unassigned defines)
  (cond ((null? defines) '())
	(else (cons
	       (list (car defines) ''*unassigned*)
	       (procedure-defines-unassigned (cdr defines))))))

(define (procedure-bodies mapping)
  (cond ((null? mapping) '())
	(else (cons
	       (cadar mapping)
	       (procedure-bodies (cdr mapping))))))

(define (procedure-assignment mapping)
  (cond ((null? mapping) '())
	(else
	 (cons (list
		'set!
		(car (procedure-defines mapping))
		(car (procedure-bodies mapping)))
	       (procedure-assignment (cdr mapping))))))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object))
  (newline))

(define current-problem '())

(define (try-again) (current-problem))

(define (ambeval-helper expr)
  (display ";;; Starting a new problem ")
  (newline)
  (ambeval expr the-global-environment
           ;; success
           (lambda (value next-alternative)
	     (set! current-problem next-alternative)
	     value)
           ;; failure
           (lambda () (error ";;; There are no more values of: " expr))))

;; procedure installation
(ambeval-helper
 '(begin
    (define (require p)
      (if (not p) (amb)))

    (define (an-element-of items)
      (require (not (null? items)))
      (amb (car items) (an-element-of (cdr items))))

    (define (an-integer-starting-from n)
      (amb n (an-integer-starting-from (+ n 1))))

    (define (prime-sum-pair list1 list2)
      (let ((a (an-element-of list1))
            (b (an-element-of list2)))
	(require (prime? (+ a b)))
	(list a b)))

    ;; prime? installation
    (define (square x) (* x x))

    (define (divides? a b)
      (= (remainder b a) 0))

    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) 
             n)
            ((divides? test-divisor n) 
             test-divisor)
            (else (find-divisor 
		   n 
		   (+ test-divisor 1)))))

    (define (prime? n)
      (= n (smallest-divisor n)))

    (define (smallest-divisor n)
      (find-divisor n 2))
    
    ;; boolean procedures
    (define (or a b)
      (cond ((eq? a true) true)
	    ((eq? b true) true)
	    (else false)))
    (define (and a b)
      (cond ((eq? a false) false)
	    ((eq? b false) false)
	    (else true)))

    ;; utilities
    (define (distinct? items)
      (cond ((null? items) true)
            ((null? (cdr items)) true)
            ((member (car items) (cdr items)) false)
            (else (distinct? (cdr items)))))

    )
 )
