#lang sicp

(define (map-classic proc . args)
  (if (null? (car args))
      '()
      (cons
       (apply proc (map car args))
       (apply map-classic
              (cons proc (map cdr args))))))

(define (filter predicate items)
    (cond ((null? items) nil)
          ((predicate (car items))
           (cons (car items) (filter predicate (cdr items))))
          (else (filter predicate (cdr items)))))

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp)
         (analyze-quoted exp))
        ((variable? exp)
         (analyze-variable exp))
        ((assignment? exp)
         (analyze-assignment exp))
        ((definition? exp)
         (analyze-definition exp))
        ((if? exp)
         (analyze-if exp))
        ((lambda? exp)
         (analyze-lambda exp))
        ((begin? exp)
         (analyze-sequence (begin-actions exp)))
        ((cond? exp)
         (analyze (cond->if exp)))
        ((application? exp)
         (analyze-application exp))
        (else
         (error "Неизвестный тип выражения -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assingnment-variable exp))))
    (lambda (env)
      (set-variable0value! var (vproc env) env))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

;(define (eval exp env)
 ; (cond ((self-evaluating? exp) exp)
  ;      ((variable? exp) (lookup-variable-value exp env))
   ;     ((quoted? exp) (text-of-quotation exp))
    ;    ((assignment? exp) (eval-assignment exp env))
     ;   ((definition? exp) (eval-definition exp env))
      ;  ((if? exp) (eval-if exp env))
       ; ((lambda? exp)
        ; (make-procedure (lambda-parameters exp)
         ;                (lambda-body exp)
          ;               env))
;        ((begin? exp)
 ;        (eval-sequence (begin-actions exp) env))
  ;      ((cond? exp) (eval (cond->if exp) env))
   ;     ((and? exp) (eval (and->if exp) env))
    ;    ((let? exp) (let->combination exp env))
     ;   ((application? exp)
      ;   (apply-interpretator (eval (operator exp) env)
       ;         (list-of-values (operands exp) env)))
        ;(else
         ;(error "Неизвестный тип выражения -- EVAL" exp))))

;(define (eval2 exp env)
;  (cond ((self-evaluating? exp) exp)
;        ((variable? exp) (lookup-variable-value exp env))
;        ((get (operator exp)) ((get (operator exp)) operands exp env))
 ;       ((application? exp)
  ;       (apply (eval (operator exp) env)
   ;             (list-of-values (operands exp) env)))
    ;    (else
      ;    (error "Неизвестный тип выражения -- EVAL" exp))))

;(define (install-eval-package)
 ; (define (quoted exp env) (text-of-quotation exp env))
  ;(define (assignment exp env) (eval-assignment exp env))
  ;(define (definition exp env) (eval-definition exp env))
  ;(define (if exp env) (eval-if exp env))
  ;(define (lambda exp env) (make-procedure (lambda-parameters exp)
   ;                                        (lambda-body exp)
    ;                                       env))
  ;(define (begin exp env) (eval-sequence (begin-actions exp) env))
  ;(define (cond exp env))

  ;(put 'quote quoted)
  ;(put 'set! assignment)
  ;(put 'define definition)
  ;(put 'if if)
  ;(put 'lambda lambda)
  ;(put 'cond cond))

(define (apply-interpretator procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Неизвестный тип процедуры -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternate exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (list-of-values-left exps env)
  (if (no-operands? exps)
      (list)
      (let ((fst-value (eval (first-operand exps) env)))
        (cons fst-value
              (list-of-values (rest-operands exps) env)))
      ))


(define (list-of-values-right exps env)
  (if (no-operands? exps)
      (list)
      (let ((rest (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest))
      ))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternate exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; нет ветви else
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "Ветвь ELSE не последняя -- COND-IF"
                       clauses))
            (make-if (cond-predicate first)
                     (if (with-consumer? first)
                         (apply-interpretator (consumer first) (cond-predicate first))
                         (sequence->exp (cond-actions first)))
                     (expand-clauses rest))))))


(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

(define (with-consumer? clause) (eq? (car (cond-actions clause)) '=>))
(define (consumer clause) (cadr (cond-actions clause)))

(define (expand-and-clauses clauses)
  (if (last-clause-and? clauses)
      (make-if (car clauses)
               (car clauses)
               'false)
      (make-if (car clauses)
               (expand-and-clauses (cdr clauses))
               'false)))

(define (and? exp) (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))
(define (last-clause-and? exp) (null? (cdr exp)))


(define (or->if exp)
  (expand-and-clauses (or-clauses exp)))

(define (expand-or-clauses clauses)
  (if (last-clause-or? clauses)
      (make-if (car clauses)
               (car clauses)
               'false)
      (make-if (car clauses)
               (car clauses)
               (expand-and-clauses (cdr clauses)))))

(define (or? exp) (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))
(define (last-clause-or? exp) (null? (cdr exp)))

(define (let->combination exp env)
  (apply-interpretator
   (eval (make-lambda (let-var-names exp)
                (let-body exp)) env)
   (list-of-values (let-var-values exp) env)))

(define (let? exp) (tagged-list? exp 'let))
(define (let-clauses exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-var-names exp) (map car (let-clauses exp)))
(define (let-var-values exp) (map cadr (let-clauses exp)))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (scan-out-defines (caddr p)))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

;(define (make-frame2 variables values)
 ; (map-classic cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;(define (add-binding-to-frame2! var val frame)
 ; (set-car! frame (cons (cons var val) (car frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Получено слишком много аргументов" vars vals)
          (error "Получено слишком мало аргументов" vars vals))))

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

(define (scan-out-defines body)
  (defines->let body))

(define (defines->let body)
  (let ((definitions-in-body (definitions body)))
    (if (null? definitions-in-body)
        body
        (list (list
         'let
         (map (lambda (el) (list el ''*unassigned*)) (definition-vars definitions-in-body))
         (make-begin (append (map-classic (lambda (val var) (list 'set! val var))
                                          (definition-vars definitions-in-body)
                                          (definition-vals definitions-in-body))
                             (body-without-definitions body))
         ))))))

(define (definitions body)
  (filter definition? body))

(define (body-without-definitions body)
  (filter (lambda (el) (not (definition? el))) body))

(define (definition-vars definitions)
  (map definition-variable definitions))

(define (definition-vals definitions)
  (map definition-value definitions))

;(define (lookup-variable-value2 var env)
 ; (define (env-loop env)
  ;  (define (frame-loop frame)
   ;     (cond ((null? frame) (env-loop (enclosing-environment env)))
    ;          ((eq? var (car (car frame))) (cadr (car frame)))
     ;         (else (frame-loop (cdr frame)))))
    ;(if (eq? env the-empty-environment)
     ;   (error "Несвязанная переменная" var)
      ;  (frame-loop (first-frame env))))
  ;(env-loop env))

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

;(define (set-variable-value2! var val env)
 ; (define (env-loop env)
  ;  (define (frame-loop frame)
   ;     (cond ((null? frame) (env-loop (enclosing-environment env)))
    ;          ((eq? var (car (car frame))) (set-cdr! (car frame) (list val)))
     ;         (else (frame-loop (cdr frame)))))
    ;(if (eq? env the-empty-environment)
     ;   (error "Несвязанная переменная" var)
      ;  (frame-loop (first-frame env))))
  ;(env-loop env))

;(define (define-variable2! var val env)
 ; (let ((frame (first-frame env)))
  ;  (define (frame-loop frame)
   ;   (cond ((null? frame)
    ;         (add-binding-to-frame2! var val first-frame))
     ;       ((eq? var (car (car frame)))
      ;       (set-cdr! (car frame) (list val)))
       ;     (else (frame-loop (cdr frame)))
        ;    ))
    ;(frame-loop first-frame)))

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

;(define (find-binding var env)
 ; (define (env-loop env)
  ;  (define (frame-loop frame)
   ;     (cond ((null? frame) (env-loop (enclosing-environment env)))
    ;          ((eq? var (car (car frame))) (cadr (car frame)))
     ;         (else (frame-loop (cdr frame)))))
    ;(if (eq? env the-empty-environment)
     ;   false
      ;  (frame-loop (first-frame env))))
  ;(env-loop env))

;(define (set-binding val binding)
 ; (set-cdr! binding (list val)))

;(define (add-binding var val env)
 ; (let ((frame (first-frame env)))
  ;  (add-binding-to-frame! var val frame)))

;(define (lookup-variable-value3 var env)
 ; (let ((binding (find-binding var env)))
  ;  (if (binding)
   ;     (cadr binding)
    ;    (error "Несвязанная переменная" var))))

;(define (set-variable-value3! var val env)
 ; (let ((binding (find-binding var env)))
  ;  (if (binding)
   ;     (set-binding val binding)
    ;    (error "Несвязанная переменная" var))
    ;))

;(define (define-variable3! var val env)
 ; (let ((binding (find-binding var env)))

  ;  (if (binding)
   ;     (set-binding val binding)
    ;    (add-binding var val env)
     ;   )))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)))

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

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))



(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; Ввод M-Eval:")
(define output-prompt ";;; Значение M-Eval:")

(define (driver-loop)
 (prompt-for-input input-prompt)
 (let ((input (read)))
   (let ((output (eval input the-global-environment)))
     (announce-output output-prompt)
     (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(driver-loop)

; 4.19
;((lambda (a)
;   (set! a 1)
;   ((lambda (f)
;      (set! f (lambda (x)
;                ((lambda (b a)
;                   (set! b (+ a x))
;                   (set! a 5)
;                   (+ a b)) '*unassigned* '*unassigned*)))
;      (f 10)) '*unassigned*)) '*unassigned*)




