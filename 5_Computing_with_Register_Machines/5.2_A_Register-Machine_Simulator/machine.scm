(define (make-register name)
  (let ((contents '*unassigned*)
        (trace-on #f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (when trace-on
                 (display "Register: ") (display name) (display ", ")
                 (display "Old value: ") (display contents) (display ", ")
                 (display "New value: ") (display value) (newline))
               (set! contents value)))
            ((eq? message 'trace-toggle)
             (lambda ()
               (set! trace-on (not trace-on))
               (display "trace: ")
               (display trace-on)
               (newline)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack-table)
  (let ((machine '())
        (stack-table '()))
    (define (initialize)
      (if (not (null? machine))
          (for-each
           (lambda (register)
             ((lookup-stack (car register)) 'initialize))
           (machine 'get-register-table))
          (error "Unknown machine -- stack register table"))
      'stack-initialize-done)
    (define (lookup-stack name)
      (let ((val (assoc name stack-table)))
        (if val
            (cdr val)
            (let ((stack (make-stack)))
              (stack 'initialize)
              ((stack 'name) name)
              (set! stack-table
                    (cons
                     (cons name stack)
                     stack-table))
              (lookup-stack name)))))
    (define (print-statistics)
      (for-each (lambda (stack)
                  (display (car stack))
                  ((lookup-stack (car stack)) 'print-statistics)
                  (newline))
                stack-table))
    (define (dispatch m)
      (cond ((eq? m 'set-machine)
             (lambda (x) (set! machine x)))
            ((eq? m 'initialize) (initialize))
            ((eq? m 'lookup-stack) lookup-stack)
            ((eq? m 'print-statistics) print-statistics)
            (else "Unknown operation -- stack register table: " m)
            ))
    dispatch))

(define (make-stack)
  (let ((s '())
        (name '())
	(number-pushes 0)
	(max-depth 0)
	(current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP" name)
          (let ((top (car s)))
            (set! s (cdr s))
	    (set! current-depth (- current-depth 1))
            top)))
    (define (top)
      (car s))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
		     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'top) (top))
            ((eq? message 'name) (lambda (x) (set! name x)))
            ((eq? message 'initialize) (initialize))
	    ((eq? message 'print-statistics)
                  (print-statistics))
            (else (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (top stack)
  (stack 'top))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    (((machine 'stack) 'set-machine) machine)
    ((machine 'stack) 'initialize)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack-table (make-stack-table))
        (the-instruction-sequence '())
        (instruction-count 0)
        (trace-on #f))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack-table 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack-table 'print-statistics)))
                 (list 'print-instruction-count
                       (lambda ()
                         (let ((c instruction-count))
                           (set! instruction-count 0)
                           c)))
                 (list 'trace-on (lambda () (set! trace-on #t)))
                 (list 'trace-off (lambda () (set! trace-on #f)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiple defined register: " name)
            (set! register-table
              (cons (list name (make-register name))
                    register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (begin
                (allocate-register name)
                (lookup-register name)))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (if trace-on (begin (display (caar insts)) (newline)))
                (set! instruction-count (+ 1 instruction-count))
                ((instruction-execution-proc (car insts)))
                (if (breakpoint? (caar insts))
                    (print-breakpoint (caar insts))
                    (execute))))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'proceed)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'stat)
               (machine-stat dispatch))
              ((eq? message 'the-instruction-sequence)
               the-instruction-sequence)
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register-table) register-table)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack-table)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE: " message))))
      dispatch)))

(define (symbol< a b)
  (string< (symbol->string a) (symbol->string b)))

(define (unique list)
 (define (unique-iter list result)
   (cond ((null? list) (sort result symbol<))
         ((null? result)
          (unique-iter (cdr list) (cons (car list) result)))
         ((not (eq? (car list) (car result)))
          (unique-iter (cdr list) (cons (car list) result)))
         (else (unique-iter (cdr list) result))))
 (unique-iter (sort list symbol<) '()))

(define (goto-reg? instruction)
  (and (eq? (caar instruction) 'goto)
       (eq? (caadr (car instruction)) 'reg)))

(define (goto-reg-instructions instructions)
  (filter goto-reg? instructions))

(define (save-restore? instruction)
  (or (eq? (caar instruction) 'save)
      (eq? (caar instruction) 'restore)))

(define (stack-operations instructions)
  (unique (map (lambda (x)
                 (cadar x))
               (filter save-restore? instructions))))

(define (assign? instruction)
  (eq? (caar instruction) 'assign))

(define (assign-sources instructions)
  (sort
   (map (lambda (x)
          (cdar x)) (filter assign? instructions))
   (lambda (x y)
     (symbol< (car x) (car y)))))

(define (machine-stat machine)
  (define instructions (machine 'the-instruction-sequence))
  (define (entry-points instructions)
    (unique (map
      (lambda (inst)
        (cadr (cadr (car inst))))
      (goto-reg-instructions instructions))))
  (define (instruction-types instructions)
    (cond ((null? instructions) '())
          (else (cons
                 (caaar instructions)
                 (instruction-types (cdr instructions))))))
  (lambda (m)
    (cond ((eq? m 'instructions-sorted)
           (unique (instruction-types instructions)))
          ((eq? m 'entry-points)
           (entry-points instructions))
          ((eq? m 'stack-operations)
           (stack-operations instructions))
          ((eq? m 'assign-sources)
           (assign-sources instructions))
          (else (error "Unknown selector -- machine-stat: " m)))))

(define (start machine)
  (machine 'start))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine pc flag stack ops)))
     insts)))

(define (assemble controller-text machine)
  (extract-labels
   (transform-syntax controller-text)
   (lambda (insts labels)
     (update-insts! insts labels machine)
     insts)
   '()))

(define (transform-syntax controller-text)
  (cond ((null? controller-text) '())
        ((tagged-list? (car controller-text) 'while)
         (append (while->machine (car controller-text))
                 (transform-syntax (cdr controller-text))))
        ((eq? (car controller-text) 'end-while)
         (append (end-while->machine)
                 (transform-syntax (cdr controller-text))))
        (else (append (list (car controller-text)) (transform-syntax (cdr controller-text))))))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (extract-labels text receive previous-label)
  (if (null? text)
      (receive '() '())
      (let* ((next-inst (car text))
             (label? (symbol? next-inst)))
        (extract-labels
         (cdr text)
         (lambda (insts labels)
           (if label?
               (if (assoc next-inst labels)
                   (error "Duplicate label -- ASSEMBLE" next-inst)
                   (receive
                       insts
                       (cons (make-label-entry next-inst insts) labels)))
               (receive
                   (cons
                    (make-instruction
                     (if (not (null? previous-label))
                         (append (list 'label previous-label) (list next-inst))
                         ; make mutable pair for debugger
                         (cons (car next-inst) (cdr next-inst))))
                    insts)
                   labels)))
         (if label? next-inst '())))))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond ((label-exp? inst)
         (make-execution-procedure (caddr inst) labels machine pc flag stack ops))
        ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'unless)
         (make-unless inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else
         (error "Unknown instruction type -- ASSEMBLE" inst))))

(define (while-gensym-stack)
  (define stack '())
  (define (push)
    (set! stack (cons (gensym) stack))
    (car stack))
  (define (pop)
    (if (null? stack)
        (error "Empty stack: most probably bad syntax -- while-gensym-stack")
        (let ((top (car stack)))
          (set! stack (cdr stack))
          top)))
  (define (get)
    (if (null? stack)
        (error "Empty stack: most probably bad syntax -- while-gensym-stack")
        (car stack)))
  (lambda (m)
    (cond ((eq? m 'push) (push))
          ((eq? m 'pop) (pop))
          ((eq? m 'get) (get))
          (else (error "Unknown operation -- while-gensym-stack" m)))))

(define (get-while-gensym-stack)
  (define the-stack (while-gensym-stack))
  (lambda () the-stack))

(define machine-while-gensym-stack
  (get-while-gensym-stack))

(define (push-while)
  ((machine-while-gensym-stack) 'push))

(define (pop-while)
  ((machine-while-gensym-stack) 'pop))

(define (make-while)
  (symbol-append 'while ((machine-while-gensym-stack) 'get)))

(define (make-end-while)
  (symbol-append 'end-while ((machine-while-gensym-stack) 'get)))

(define (while->machine text)
  (push-while)
  (list (make-while)
        (append (list 'unless) (cdr text))
        (list 'branch (list 'label (make-end-while)))))

(define (end-while->machine)
  (let ((expr (list (list 'goto (list 'label (make-while)))
                    (make-end-while))))
    (pop-while)
    expr))

(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (make-unless inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (not (condition-proc)))
            (advance-pc pc)))
        (error "Bad UNLESS instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label
                   labels
                   (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst)))
        (s (((machine 'stack) 'lookup-stack) (stack-inst-reg-name inst))))
    (lambda ()
      (push s (cons (stack-inst-reg-name inst) (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst)))
        (s (((machine 'stack) 'lookup-stack) (stack-inst-reg-name inst))))
    (lambda ()
      (begin
        (set-contents! reg (cdr (pop s)))
        (advance-pc pc)))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type -- ASSEMBLE" exp))))

(define false #f)

(define true (not false))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (not (label-exp? e))
                    (make-primitive-exp e machine labels)
                    (error "Cannot apply operation on label -- ASSEMBLE" (cadr e))))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

;;; Debugger

(define (set-breakpoint machine label offset)
  (define (install-breakpoint inst)
    (unless (and (pair? (car inst)) (eq? 'break-point (caar inst)))
      (let ((rest (cons (car inst) (cdr inst))))
        (set-car! inst (list 'break-point label offset))
        (set-cdr! inst (list rest)))))
  (define (instruction-iterator inst-seq found-label? n)
    (cond ((null? inst-seq)
           (error "Unknown offset or label -- set-breakpoint" label))
          ((= n 0)
           (error "Cannot install breakpoint before label -- set-breakpoint")))
    (let* ((inst (if (breakpoint? (caar inst-seq))
                     (cadr (caar inst-seq))
                     (caar inst-seq)))
           (label? (and (label-exp? inst) (eq? label (cadr inst)))))
      (cond
       ((and (not label?) (not found-label?)) ; Find label
        (instruction-iterator (cdr inst-seq) found-label? n))
       ((and label? (not found-label?)) ; Label found, proceed to seek offset
        (instruction-iterator inst-seq label? n))
       ((and found-label? (> n 1)) ; Seek offset
        (instruction-iterator (cdr inst-seq) found-label? (- n 1)))
       ((and found-label? (= n 1)) ; Install the breakpoint
        (install-breakpoint (caar inst-seq))))))
  (instruction-iterator (machine 'the-instruction-sequence) #f offset))

(define (breakpoint? inst)
  (and (pair? (car inst)) (eq? 'break-point (caar inst))))

(define (print-breakpoint inst)
  (let ((breakpoint (car inst)))
    (display (string-append
              "breakpoint -> "
              (symbol->string (cadr breakpoint))
              " "
              (number->string (caddr breakpoint))))
    (newline)))

(define (proceed-machine machine)
  (machine 'proceed))

(define (cancel-breakpoint machine label n)
  (define (instruction-iterator inst-seq)
    (cond ((null? inst-seq)
           (error "Unknown breakpoint with label at offset -- cancel-breakpoint"
                  label (number->string n)))
          ((and (breakpoint? (caar inst-seq))
                (eq? label (cadr (caaar inst-seq)))
                (eq? n (caddr (caaar inst-seq))))
           (let ((first (caar (cdaar inst-seq)))
                 (rest (cdar (cdaar inst-seq)))
                 (struct (caar inst-seq)))
             (set-car! struct first)
             (set-cdr! struct rest)
             'done))
          (else (instruction-iterator (cdr inst-seq)))))
  (instruction-iterator (machine 'the-instruction-sequence)))

(define (cancel-all-breakpoints machine)
  (define (instruction-iterator inst-seq)
    (cond ((null? inst-seq)
           'done)
          ((breakpoint? (caar inst-seq))
           (let ((first (caar (cdaar inst-seq)))
                 (rest (cdar (cdaar inst-seq)))
                 (struct (caar inst-seq)))
             (set-car! struct first)
             (set-cdr! struct rest)
             (instruction-iterator (cdr inst-seq))))
          (else (instruction-iterator (cdr inst-seq)))))
  (instruction-iterator (machine 'the-instruction-sequence)))
