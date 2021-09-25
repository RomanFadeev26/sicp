#lang sicp

#|(controller
 (assign produce (const 1))
 (assign count (const 1))
 
 test-factorial
 (test (op >) (reg count) (reg n))
 (branch (label factorial-done))
 (assign produce (op *) (reg produce) (reg count))
 (assign count (op +) (reg count) (const 1))
 (goto (label test-factorial))
 factorial-done)

(controller
 sqrt-loop
 (assign x (op read))
 (assign guess (const 1.0))
 
 start-iter
 (test (op good-enough?) (reg guess))
 (branch (label sqrt-done))
 (assign guess (op improve) (reg guess))
 (goto (label start-iter))
 
 sqrt-done
 (perform (op print) (req guess))
 (goto (label sqrt-loop)))

(controller
 sqrt-loop
 (assign x (op read))
 (assign guess (const 1.0))
 
 start-iter
 (assign t1 (op square) (reg guess))
 (assign t2 (op -) (reg x) (reg t1))
 (assign t3 (op abs) (reg t3))
 (test (op <) (reg t3) (const 0.001))
 (branch (label sqrt-done))
 (assign t4 (op /) (reg x) (reg guess))
 (assign guess (op average) (reg guess) (reg t4))
 (goto (label start-iter))
 
 sqrt-done
 (perform (op print) (req guess))
 (goto (label sqrt-loop)))

(controller
 (assign continue (label expt-done))
 expt-loop
 (test (op =) (reg n) (const 0))
 (branch (label base-case))
 (save continue)
 (assign n (op -) (reg n) (const 1))
 (assign continue (label after-expt))
 (goto (label expt-loop))
 after-expt
 (restore continue)
 (assign val (op *) (reg b) (reg val))
 base-case
 (goto (reg continue))
 expt-done)

(controller
 (assign product (const 1))
 expt-loop
 (test (op =) (reg n) (const 0))
 (branch (label expt-done))
 (assign n (op -) (reg n) (const 1))
 (assign product (op *) (reg b) (reg product))
 (goto (label expt-loop))
 expt-done
 )

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))
|#

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (for-each proc items)
  (if (null? items)
      (newline)
      ((lambda () (proc (car items)) (for-each proc (cdr items))))))



(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Неизвестная операция -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '())
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
          (error "Пустой стек -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
            (cond ((eq? message 'push) push)
                  ((eq? message 'pop) (pop))
                  ((eq? message 'initialize) (initialize))
                  ((eq? message 'print-statistics)
                   (print-statistics))
                  (else (error "Неизвестная операция -- STACK"
                               message))))
    dispatch))



(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))


#| (define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stacks (list (list 'pc (make-stack))
                      (list 'flag (make-stack))))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (for-each (lambda (stack) ((cadr stack) 'initialize)) stacks)))
                 (list 'print-stacks-statistics
                       (lambda () (for-each (lambda (stack) (begin (display (car stack))(newline)((cadr stack) 'print-statistics))) stacks)))))
           (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Многократно определённый регистр: " name)
            (begin (set! register-table
                         (cons (list name (make-register name))
                               register-table))
                   (set! stacks
                         (cons (list name (make-stack))
                               stacks)))
            )
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Неизвестный регистр: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stacks) stacks)
              ((eq? message 'operations) the-ops)
              (else (error "Неизвестная операция -- MACHINE" message))))
      dispatch))) |#

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
           (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Многократно определённый регистр: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table))
            )
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Неизвестный регистр: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Неизвестная операция -- MACHINE" message))))
      dispatch)))


(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (if (assoc next-inst labels)
                                  (error "Дублирующая метка -- ASSEMBLE" next-inst)
                                  (receive insts
                                           (cons (make-label-entry next-inst
                                                                   insts)
                                                 labels)))
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                       labels)))))))

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
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Неопределённая метка -- ASSEMBLE" label-name))))

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
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
        (else (error "Неизвестный тип команды -- ASSEMBLE" inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
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
        (error "Плохая команда TEST -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Плохая команда BRANCH -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels
                                      (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Плохая команда GOTO -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                               (stack-inst-reg-name inst))))
        (lambda ()
          (push stack (get-contents reg))
          (advance-pc pc)))
  )

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))
    ))

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
        (error "Плохая команда PERFORM -- ASSEMBLE" inst))))

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
        (else
         error "Неизвестный тип выражения -- ASSEMBLE" exp)))

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
                (make-primitive-exp e machine labels))
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
        (error "Неизвестная операция -- ASSEMBLE" symbol))))

#|
(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))


(define expt-rec-machine
  (make-machine
   '(n val continue b)
   (list (list '- -) (list '= =) (list '* *))
   '((assign continue (label expt-done))
    expt-loop
    (test (op =) (reg n) (const 0))
    (branch (label base-case))
    (save continue)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-expt))
    (goto (label expt-loop))
    after-expt
    (restore continue)
    (assign val (op *) (reg b) (reg val))
    base-case
    (goto (reg continue))
    expt-done)))

(define expt-iter-machine
  (make-machine
   '(n product b)
   (list (list '- -) (list '= =) (list '* *))
   '((assign product (const 1))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label expt-done))
     (assign n (op -) (reg n) (const 1))
     (assign product (op *) (reg b) (reg product))
     (goto (label expt-loop))
     expt-done
     )))


(controller
 (assign product (const 1))
 expt-loop
 (test (op =) (reg n) (const 0))
 (branch (label expt-done))
 (assign n (op -) (reg n) (const 1))
 (assign product (op *) (reg b) (reg product))
 (goto (label expt-loop))
 expt-done
 )



(define expt-rec-machine
  (make-machine
   '(n val continue b)
   (list (list '- -) (list '= =) (list '* *))
   '((assign continue (label expt-done))
    expt-loop
    (test (op =) (reg n) (const 0))
    (branch (label base-case))
    (save continue)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-expt))
    (goto (label expt-loop))
    after-expt
    (restore continue)
    (assign val (op *) (reg b) (reg val))
    base-case
    (goto (reg continue))
    expt-done)))


;; 5.3

(define count-leaves-rec-machine
  (make-machine
   '(tree continue count val)
   (list (list '+ +) (list 'null? null?) (list 'pair? pair?))
   '((assign continue (label count-leaves-done))
    count-leaves-loop
    
    (test (op null?) (reg tree))
    (branch (label base-case))
    
    (test (op pair?) (reg tree))
    (branch (label to-recursion))
    (assign count (const 1))
    (goto (reg continue))
    
    to-recursion
    (save continue)
    (assign continue (label after-count-leaves-1))
    (save tree)
    (assign tree (op car) (reg tree))
    (goto (label count-leaves-loop))
    
    after-count-leaves-1
    (restore tree)
    (restore continue)
    (assign tree (op cdr) (reg tree))
    (save continue)
    (assign continue (label after-count-leaves-2))
    (save count)
    (goto (label count-leaves-loop))
    after-count-leaves-2
    (assign val (reg count))
    (restore count)
    (restore continue)
    (assign count (op +)
            (reg val)
            (reg count))
    (goto (reg continue))
    
    base-case
    (assign count (const 0))
    (goto (reg continue))
    expt-done)))

(define count-leaves-iter-machine
  (make-machine
   '(tree continue count val)
   (list (list '+ +) (list 'null? null?) (list 'pair? pair?))
   '((assign count (const 0))
     (assign continue (label count-leaves-done))
     
     count-leaves-loop
     (test (op null?) (reg tree))
     (goto (reg continue))

     (test (op pair?) (reg tree))
     (branch (label to-right-branch))
     
     (assign count (op +) (reg count)(const 1))
     (goto (reg continue))

     to-right-branch
     (save continue)
     (assign continue (label to-left-branch))
     (save tree)
     (assign tree (op cdr) (reg tree))
     (goto (label count-leaves-loop))

     to-left-branch
     (restore tree)
     (restore continue)
     (assign tree (op car) (reg tree))
     (goto (label count-leaves-loop))
     
     count-leaves-done
     )))

|#



#|'(begin-garbage-collection
   (assign free (const 0))
   (assign scan (const 0))
   (assign old (reg root))
   (assign relocate-continue (label reassign-root))
   (goto (label relocate-old-result-in-new))
   reassign-root
   (assign root (reg new))
   (goto (label gc-loop))
   gc-loop
   (test (op =) (reg scan) (reg free))
   (branch (label gc-flip))
   (assign old (op vector-ref) (reg new-cars) (reg scan))
   (assign relocate-continue (label update-car))
   (goto (label relocate-old-result-in-new))
   update-car
   (perform
    (op vector-set!) (reg new-cars) (reg scan))
   (assign old (op vector-ref) (reg new-cdrs) (reg scan))
   (assign relocate-continue (label update-cdr))
   (goto (label relocate-old-result-in-new))
   update-cdr
   (perform
    (op vector-set!) (reg new-cdrs) (reg scan) (reg new))
   (assign scan (op +) (reg scan) (const 1))
   (goto (label gc-loop))
   relocate-old-result-in-new
   (test (op pointer-to-pair?) (reg old))
   (branch (label pair))
   (assign new (reg old))
   (goto (reg relocate-continue))
   pair
   (assign oldcr (op vector-ref) (reg the-cars) (reg old))
   (test (op broken-heart?) (reg oldcr))
   (branch (label already-moved))
   (assign new (reg free))
   (assign free (op +) (reg free) (const 1))
   (perform (op vector-set!)
            (reg new-cars (reg new) (reg oldcr)))
   (assign oldcr (op vector-ref) (reg the-cdrs) (reg old))
   (perform (op vector-set!)
            (reg new-cdrs) (reg new) (reg oldcr))
   (perform (op vector-set!)
            (reg the-cars) (reg old) (const broken-heart))
   (perform (op vector-set!)
            (reg the-cdrs) (reg old) (reg new))
   (goto (reg relocate-continue))
   already-moved
   (assign new (op vector-ref) (reg the-cdrs) (reg old))
   (goto (reg relocate-continue))
   gc-flip
   (assign temp (reg the-cdrs))
   (assign the-cdrs (reg new-cdrs))
   (assign new-cdrs (reg temp))
   (assign temp (reg the-cars))
   (assign the-cars (reg new-cars))
   (assign new-cars (reg temp))) |#

;; 5.4

(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))

(define (make-frame variables values)
  (cons variables values))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Получено слишком много аргументов" vars vals)
          (error "Получено слишком мало аргументов" vars vals))))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define primitive-procedures
  (list (list '* *)
        (list '> >)
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list 'map map)
        (list 'read read)))

(define the-empty-environment '())

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

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

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (if? exp) (tagged-list? exp 'if))

(define (begin? exp) (tagged-list? exp 'begin))

(define (cond? exp) (tagged-list? exp 'cond))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let-clauses exp) (cadr exp))

(define (let-body exp) (cddr exp))
(define (let-var-names exp) (map car (let-clauses exp)))
(define (let-var-values exp) (map cadr (let-clauses exp)))

(define (let->combination exp)
  (cons (make-lambda (let-var-names exp)
                (let-body exp)) (let-var-values exp)))

(define (let? exp) (tagged-list? exp 'let))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (get-global-environment) the-global-environment)

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (application? exp) (pair? exp))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (let ((value (car vals)))
               (if (eq? value '*unassigned*)
                   (error "Нет значения переменной -- LOOKUP-VARIABLE-VALUE" var)
                   value)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Несвязанная переменная" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (text-of-quotation exp) (cadr exp))


(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define apply-in-underlying-scheme apply)

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (procedure-environment p) (cadddr p))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))
(define (true? x)
  (not (eq? x false)))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Несвязанная переменная -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (last-condition? cond-cases) (null? (cdr cond-cases)))
(define (first-condition cond-cases) (car cond-cases))

(define (rest-conditions cond-cases) (cdr cond-cases))

(define (cases cond-exp)
  (cdr cond-exp))

(define (cond-predicate cond-case)
  (car cond-case))

(define (cond-consequent cond-case) (cadr cond-case))

(define eceval-operations
  (list
   (list '* *)
   (list '> >)
   (list 'procedure-body procedure-body)
   (list 'extend-environment extend-environment)
   (list 'procedure-parameters procedure-parameters)
   (list 'compound-procedure? compound-procedure?)
   (list 'adjoin-arg adjoin-arg)
   (list 'empty-arglist empty-arglist)
   (list 'let->combination let->combination)
   (list 'get-global-environment get-global-environment)
   (list 'cond-consequent cond-consequent)
   (list 'cond-predicate cond-predicate)
   (list 'cases cases)
   (list 'rest-conditions rest-conditions)
   (list 'first-condition first-condition)
   (list 'last-condition? last-condition?)
   (list 'car car)
   (list 'cdr cdr)
   (list 'cons cons)
   (list 'null? null?)
   (list '+ +)
   (list 'map map)
   (list 'read read)
   (list 'self-evaluating? self-evaluating?)
   (list 'variable? variable?)
   (list 'quoted? quoted?)
   (list 'assignment? assignment?)
   (list 'definition? definition?)
   (list 'lambda? lambda?)
   (list 'if? if?)
   (list 'begin? begin?)
   (list 'cond? cond?)
   (list 'let? let?)
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   (list 'application? application?)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'text-of-quotation text-of-quotation)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'make-procedure make-procedure)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)
   (list 'last-operand? last-operand?)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'procedure-environment procedure-environment)
   (list 'begin-actions begin-actions)
   (list 'last-exp? last-exp?)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'true? true?)
   (list 'if-alternative if-alternative)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'define-variable! define-variable!)
   ))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   '(
     read-eval-print-loop
     (perform (op initialize-stack))
     (perform
      (op prompt-for-input) (const ";;; Ввод EC-Eval: "))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))
     print-result
     (perform
      (op print-stack-statistics))
     (perform
      (op announce-output) (const ";;; Значение EC-Eval: "))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     unknown-expression-type
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))
     unknown-procedure-type
     (restore continue)
     (assign val (const unknown-procedure-type))
     (goto (label signal-error))
     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     
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
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op cond?) (reg exp))
     (branch (label ev-cond))
     (test (op let?) (reg exp))
     (branch (label ev-let))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))
     
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
     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))
     ev-appl-did-operator
     (restore unev)
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val))
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
     ev-if
     (save exp)
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch))
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
     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev)
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     (goto (label eval-dispatch))
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
     (save unev)
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch))
     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))
     ev-cond
     (assign unev (op cases) (reg exp))
     (save unev)
     (save env)
     (save continue)
     (assign continue (label ev-cond-decide))
     ev-cond-next-condition
     (test (op last-condition?) (reg unev))
     (assign unev (op first-condition) (reg unev))
     (branch (label ev-cond-consequent))
     (assign exp (op cond-predicate) (reg unev))
     (assign continue (label ev-cond-decide))
     (goto (label eval-dispatch))
     ev-cond-decide
     (test (op true?) (reg val))
     (branch (label ev-cond-consequent))
     (restore unev)
     (assign unev (op rest-conditions) (reg unev))
     (save unev)
     (goto (label ev-cond-next-condition))
     ev-cond-consequent
     (restore env)
     (assign unev (op cond-consequent) (reg unev))
     (goto (label ev-sequence))
     ev-let
     (assign exp (op let->combination) (reg exp))
     (goto (label eval-dispatch))
     )))

(start eceval)



