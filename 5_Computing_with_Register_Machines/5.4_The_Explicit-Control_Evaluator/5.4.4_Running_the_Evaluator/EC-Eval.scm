(let ((machine-path "../../5.2_A_Register-Machine_Simulator"))
  (add-to-load-path machine-path)
  (load (string-append machine-path "/machine.scm")))

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))

(define (no-more-exps? seq) (null? seq))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (text-of-quotation exp) (cadr exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (variable? exp)
  (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (if? exp) (tagged-list? exp 'if))

(define (cond? exp) (tagged-list? exp 'cond))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (begin? exp) (tagged-list? exp 'begin))

(define (application? exp)
  (pair? exp))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

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

(define (text-of-quotation exp) (cadr exp))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

(define (let-expression-definitions exp)
  (cadr exp))

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

;;; scan out defines
(define (scan-out-defines p-body)
  (if (not (null? (procedure-variable-body-mapping p-body)))
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

(define (operands exp) (cdr exp))

(define (operator exp) (car exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (primitive-implementation proc)
  (if (procedure? proc)
      proc
      (cadr proc)))

(define (apply-primitive-procedure proc args)
  (if (eq? 1 (procedure-minimum-arity proc))
      (proc args)
      (apply
       (primitive-implementation proc) args)))

(define (procedure-parameters p) (cadr p))

(define (procedure-environment p) (cadddr p))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(define (procedure-body p) (caddr p))

(define (begin-actions exp) (cdr exp))

(define (first-exp seq) (car seq))

(define (last-exp? seq) (null? (cdr seq)))

(define (rest-exps seq) (cdr seq))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define cond-special-form-flag #f)

;;; Let machine decide which strategy to use
;;; for evaluating cond based on this boolean.
;;; #f to use derived cond->if
;;; #t to use special form
(define (cond-special-form) cond-special-form-flag)

(define (cond-clauses exp) (cdr exp))

(define (cond-predicate clause) (car clause))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-actions clause) (cdr clause))

(define (make-begin seq) (cons 'begin seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

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

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

(define (assignment-value exp) (caddr exp))

(define (assignment-variable exp) (cadr exp))

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

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
		   (cddr exp)))) ; body

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

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

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)
	(list '= =)
	(list '* *)
	(list '+ +)
	(list '- -)
    (list '< <)
    (list '> >)))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

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

(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (get-global-environment) the-global-environment)

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object))
  (newline))

(define eceval-operations
  (list (list 'self-evaluating? self-evaluating?)
        (list 'empty-arglist empty-arglist)
        (list 'adjoin-arg adjoin-arg)
        (list 'last-operand? last-operand?)
        (list 'no-more-exps? no-more-exps?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'car car)
        (list 'cdr cdr)
        (list 'null? null?)
        (list 'assignment? assignment?)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'cond? cond?)
        (list 'lambda? lambda?)
        (list 'begin? begin?)
        (list 'application? application?)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'text-of-quotation text-of-quotation)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'make-procedure make-procedure)
        (list 'operands operands)
        (list 'operator operator)
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'rest-operands rest-operands)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'compound-procedure? compound-procedure?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-environment procedure-environment)
        (list 'extend-environment extend-environment)
        (list 'procedure-body procedure-body)
        (list 'begin-actions begin-actions)
        (list 'first-exp first-exp)
        (list 'last-exp? last-exp?)
        (list 'rest-exps rest-exps)
        (list 'if-predicate if-predicate)
        (list 'if-consequent if-consequent)
        (list 'if-alternative if-alternative)
        (list 'cond->if cond->if)
        (list 'cond-special-form cond-special-form)
        (list 'cond-clauses cond-clauses)
        (list 'cond-else-clause? cond-else-clause?)
        (list 'cond-predicate cond-predicate)
        (list 'cond-actions cond-actions)
        (list 'true #t)
        (list 'false #f)
        (list 'true? true?)
        (list 'false? false?)
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        (list 'set-variable-value! set-variable-value!)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        (list 'define-variable! define-variable!)
        (list 'prompt-for-input prompt-for-input)
        (list 'announce-output announce-output)
        (list 'read read)
        (list 'get-global-environment get-global-environment)
        (list 'user-print user-print)
        ))

(define ec-eval-controller
  '(
    ;;; Running the evaluator
    read-eval-print-loop
    ;; (perform (op initialize-stack))
    ;; (perform
    ;;  (op prompt-for-input) (const ";;; EC-Eval input:"))
    ;; (assign exp (op read))
    ;; (assign env (op get-global-environment))
    (assign continue (label print-result))
    (goto (label eval-dispatch))

  ;;; The Core of the Explicit-Control Evaluator
  eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))
    (test (op variable?) (reg exp))
    (branch (label ev-variable))
    (test (op quoted?) (reg exp))
    (branch (label ev-quoted))
    (test (op assignment?) (reg exp))
    (branch (label ev-assignment))
    (test (op definition?) (reg exp))
    (branch (label ev-definition))
    (test (op if?) (reg exp))
    (branch (label ev-if))
    (test (op cond?) (reg exp))
    (branch (label ev-cond))
    (test (op lambda?) (reg exp))
    (branch (label ev-lambda))
    (test (op begin?) (reg exp))
    (branch (label ev-begin))
    (test (op application?) (reg exp))
    (branch (label ev-application))
    (goto (label unknown-expression-type))

  ;;; Simple expressions
  ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))
  ev-variable
    (assign val (op lookup-variable-value) (reg exp) (reg env))
    (goto (reg continue))
  ev-quoted
    (assign val (op text-of-quotation) (reg exp))
    (goto (reg continue))
  ev-lambda
    (assign unev (op lambda-parameters) (reg exp))
    (assign exp (op lambda-body) (reg exp))
    (assign val (op make-procedure)
            (reg unev) (reg exp) (reg env))
    (goto (reg continue))

  ;;; Evaluating procedure applications
  ev-application
    (save continue)
    (save env)
    (assign unev (op operands) (reg exp))
    (save unev)
    (assign exp (op operator) (reg exp))
    (assign continue (label ev-appl-did-operator))
    (goto (label eval-dispatch))

  ev-appl-did-operator
    (restore unev)                         ; the operands
    (restore env)
    (assign argl (op empty-arglist))
    (assign proc (reg val))                ; the operator
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)

  ev-appl-operand-loop
    (save argl)
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label ev-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label ev-appl-accumulate-arg))
    (goto (label eval-dispatch))

  ev-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))

  ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))
  ev-appl-accum-last-arg
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)
    (goto (label apply-dispatch))

  ;;; Procedure application
  apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label compound-apply))
    (goto (label unknown-procedure-type))

  primitive-apply
    (assign val (op apply-primitive-procedure)
            (reg proc)
            (reg argl))
    (restore continue)
    (goto (reg continue))

  compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env (op extend-environment)
            (reg unev) (reg argl) (reg env))
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))

  ;;; Sequence Evaluation and Tail Recursion
  ev-begin
    (assign unev (op begin-actions) (reg exp))
    (save continue)
    (goto (label ev-sequence))

  ev-sequence
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch (label ev-sequence-last-exp))
    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))
  ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))
  ev-sequence-last-exp
    (restore continue)
    (goto (label eval-dispatch))

  ;;; Conditionals, Assignments, and Definitions
  ev-if
    (save exp)                    ; save expression for later
    (save env)
    (save continue)
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label eval-dispatch))  ; evaluate the predicate
  
  ev-if-decide
    (restore continue)
    (restore env)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label ev-if-consequent))

  ev-if-alternative
    (assign exp (op if-alternative) (reg exp))
    (goto (label eval-dispatch))
  ev-if-consequent
    (assign exp (op if-consequent) (reg exp))
    (goto (label eval-dispatch))

  ev-cond
    (test (op cond-special-form))
    (branch (label ev-cond-special-form))
  cond->if
    (assign exp (op cond->if) (reg exp))
    (goto (label eval-dispatch))

  ev-cond-special-form
    (assign exp (op cond-clauses) (reg exp))
    (goto (label ev-cond-clause-iterator))

  ev-cond-clause-iterator
    (test (op null?) (reg exp))
    (branch (label ev-cond-null))
    (save exp)
    (save env)
    (save continue)
    (assign exp (op car) (reg exp)) ; first clause
    (test (op cond-else-clause?) (reg exp))
    (branch (label ev-cond-else-syntax-check))
    (save exp)
    (assign exp (op cond-predicate) (reg exp))
    (assign continue (label ev-cond-decide))
    (goto (label eval-dispatch))

  ;; if predicate is true, eval cond actions, else loop
  ev-cond-decide
    (restore exp) ; make the cond clause available
    (restore env)
    (restore continue)
    (test (op true?) (reg val))
    (branch (label ev-cond-actions))
    ;; clause not found, compute rest of cond clauses
    (restore exp)
    (assign exp (op cdr) (reg exp)) ; cdr the cond body
    (goto (label ev-cond-clause-iterator))

  ev-cond-actions
    (save continue)
    (assign unev (op cond-actions) (reg exp))
    (goto (label ev-sequence))

  ev-cond-else-syntax-check
    (restore exp) ; make cond body available
    (save exp)
    (assign exp (op cdr) (reg exp))
    (test (op null?) (reg exp)) ; if not null? 'else' is in wrong pos
    (branch (label ev-cond-else))
    (goto (label syntax-error-cond-else))

  ev-cond-else
    (restore exp) ; cond body
    (assign exp (op car) (reg exp))
    (goto (label ev-cond-actions))

  ;; No true predicate or else clause found
  ev-cond-null
    (save continue)
    (assign unev (const (#f)))
    (goto (label ev-sequence))

  ;;; Assignment and definitions
  ev-assignment
    (assign unev (op assignment-variable) (reg exp))
    (save unev)                   ; save variable for later
    (assign exp (op assignment-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-assignment-1))
    (goto (label eval-dispatch))  ; evaluate the assignment value
  ev-assignment-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform
     (op set-variable-value!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))

  ev-definition
    (assign unev (op definition-variable) (reg exp))
    (save unev)                   ; save variable for later
    (assign exp (op definition-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-definition-1))
    (goto (label eval-dispatch))  ; evaluate the definition value
  ev-definition-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform
     (op define-variable!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))

  unknown-expression-type
    (assign val (const unknown-expression-type-error))
    (goto (label signal-error))

  unknown-procedure-type
    (restore continue)    ; clean up stack (from `apply-dispatch')
    (assign val (const unknown-procedure-type-error))
    (goto (label signal-error))

  syntax-error-cond-else
    (assign val (const cond-else-clause-not-last))
    (goto (label signal-error))

  signal-error
    (perform (op user-print) (reg val))
    ; (goto (label read-eval-print-loop))

  print-result
    ;; (perform
    ;;  (op announce-output) (const ";;; EC-Eval value:"))
    (perform (op user-print) (reg val))
    ; (goto (label read-eval-print-loop))
  )
)

(define eceval
  (make-machine '() eceval-operations ec-eval-controller))

(define (eceval-exp machine exp)
  ((machine 'stack) 'initialize)
  ((((machine 'get-register) 'exp) 'set) exp)
  ((((machine 'get-register) 'env) 'set) (get-global-environment))
  (start machine))
