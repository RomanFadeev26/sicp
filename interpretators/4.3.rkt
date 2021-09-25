#lang sicp


(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between start finish)
  (require (<= start finish))
  (amb start (an-integer-between (+ 1 start) finish)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define (interleave f s)
  (amb f (interleave s f)))

;(define (a-pythagorean-triple-between-all)
;  (let ((i (interleave (an-integer-starting-from 1) (an-integer-starting-from 1))))
;    (let ((j (interleave (an-integer-starting-from 1) (an-integer-starting-from i))))
;      (let ((k (interleave (an-integer-starting-from j) (an-integer-starting-from i))))
;        (require (= (+ (* i i) (* j ))))
;        (list i j k)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (a-pythagorean-triple-between-all)
  (define (limit n)
    (let ((lim (an-integer-starting-from (+ n 1))))
      (require (and (odd? lim) (odd? n)))
      (require (= 1 (gcd lim n)))
      lim))
  (let ((n (an-integer-starting-from 1)))
    (let ((m (limit n)))
      (let ((i (* m n))
            (j (/ (- (* m m) (* n n)) 2))
            (n (/ (+ (* m m) (* n n)) 2)))
        (list i j n)))
    ))

(define (square x)
  (* x x))

(define (a-pythagorean-triple-from low)
  (define (limit i) (if (odd? i)
                        (/ (- (square i) 1) 2)
                        (- (/ (square i) 4) 1)))
  (let* ((i (an-integer-starting-from low))
         (j (an-integer-between i (limit i)))
         (k (sqrt (+ (square i) (square j)))))
    (require (integer? k))
    (list i j k)))


(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling-1)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))


(define (multiple-dwelling-2)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(define (multiple-dwelling-3)
  (let ((baker (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (let ((fletcher (amb 1 2 3 4 5)))
        (require (not (= fletcher 5)))
        (require (not (= fletcher 1)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (let ((miller (amb 1 2 3 4 5)))
          (require (> miller cooper))
          (let ((smith (amb 1 2 3 4 5)))
            (require (not (= (abs (- smith fletcher)) 1)))
            (require
              (distinct? (list baker cooper fletcher miller smith)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith)))))
      )))

(define (report elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (repeat n proc start-time time)
    (if (> n 0)
        (begin (proc) (repeat (- n 1) proc (runtime) (/ (+ (- (runtime) start-time) time) 2.0)))
        time))

(define (first-time start-time proc)
    (proc)
    (- (runtime) start-time))

(define (time-of-evaluation proc)
  (let ((start (runtime)))
    (report (repeat 10 proc start (first-time start proc))))
  )

(define (xor a b)
  (or (and a (not b))
      (and b (not a))))

(define (girls)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joahn (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require
      (distinct? (list betty ethel joahn kitty mary)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joahn 2)))
    (require (xor (= joahn 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joahn joahn)
          (list 'kitty kitty)
          (list 'mary mary))))



; 1 дочь 2 яхта
;(define (jachts)
;  (let ((mur (list 'marry-ann 'laurna))
;        (barnakl (list 'melissa 'gabriella))
;        (kholl (list '- 'rosalinda))
;        (dauning (list '- melissa)))))


(define (jachts)
  (let ((moore-daughter (amb 'marry-ann 'gabriella 'lorna 'rosalinda 'melissa))
        (moore-jacht (amb 'marry-ann 'gabriella 'lorna 'rosalinda 'melissa)))
    (require (and (eq? moore-jacht 'lorna) (eq? moore-daughter 'marry-ann)))
    (let ((barknuckle-daughter (amb 'marry-ann 'gabriella 'lorna 'rosalinda 'melissa))
          (barknuckle-jacht (amb 'marry-ann 'gabriella 'lorna 'rosalinda 'melissa)))
      (require (eq? barknuckle-jacht 'gabriella))
      (let ((kholl-jacht (amb 'marry-ann 'gabriella 'lorna 'rosalinda 'melissa)))
        (require (eq? kholl-jacht 'rosalinda))
        (let ((dauninng-jacht (amb 'marry-ann 'gabriella 'lorna 'rosalinda 'melissa)))
          (require (and (eq? dauninng-jacht barknuckle-daughter) (eq? dauninng-jacht 'melissa)))
          (let ((dauninng-daughter (amb 'marry-ann 'gabriella 'lorna 'rosalinda 'melissa))
                (kholl-daughter (amb 'marry-ann 'gabriella 'lorna 'rosalinda 'melissa))
                (parker-daughter (amb 'marry-ann 'gabriella 'lorna 'rosalinda 'melissa))
                (parker-jacht (amb 'marry-ann 'gabriella 'lorna 'rosalinda 'melissa)))
            (require (and (eq? kholl-daughter 'gabriella) (eq? kholl-jacht parker-daughter)))
            (let ((daughters (list moore-daughter dauninng-daughter kholl-daughter barknuckle-daughter parker-daughter))
                  (yachts (list moore-jacht dauninng-jacht kholl-jacht barknuckle-jacht parker-jacht)))
              (require (distinct? daughters))
              (require (distinct? yachts))
              (require (not (eq? dauninng-daughter dauninng-jacht)))
              (require (not (eq? kholl-daughter kholl-jacht)))
              (require (not (eq? parker-daughter parker-jacht)))
              (list (list 'moore moore-daughter moore-jacht)
                    (list 'dauninng dauninng-daughter dauninng-jacht)
                    (list 'kholl kholl-daughter kholl-jacht)
                    (list 'barknuckle barknuckle-daughter barknuckle-jacht)
                    (list 'parker parker-daughter parker-jacht)))))))))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))


(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))


(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))


(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))


(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))


(define (eval exp env)
  ((analyze exp) env))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (amb? exp)
  (tagged-list? exp 'amb))

(define (amb-choices exp)
  (cdr exp))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp)
         (analyze-quoted exp))
        ((require? exp)
         (analyze-require exp))
        ((variable? exp)
         (analyze-variable exp))
        ((assignment? exp)
         (analyze-assignment exp))
        ((permanent-assignment? exp)
         (analyze-permanent-assignment exp))
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
        ((let? exp)
         (analyze (let->combination exp)))
        ((amb? exp)
         (analyze-amb exp))
        ((application? exp)
         (analyze-application exp))
        (else
         (error "Неизвестный тип выражения -- ANALYZE" exp))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail) (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (scan-out-defines (lambda-body exp)))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
              fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env suceed fail)
      (proc1 env
             (lambda (proc1-value fail2)
               (proc2 env suceed fail2))
             fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Пустая последовательность -- ANALYZE-SEQUENCE" exps))
    (loop (car procs) (cdr procs))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env (lambda (val fail2)
                   (let ((old-value
                          (lookup-variable-value var env)))
                     (set-variable-value! var val env)
                     (succeed 'ok
                              (lambda ()
                                (set-variable-value! var
                                                     old-value
                                                     env)
                                (fail2)))))
             fail)
      )))

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env (lambda (val fail2)
                     (set-variable-value! var val env)
                     (succeed 'ok fail2))
             fail)
      )))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail)
      )))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;; продолжение успеха для этой aproc
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                ;; продолжение успеха для
                                ; рекурсивного вызова get-args
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (execute-application procedure arguments succeed fail)
  (cond ((primitive-procedure? procedure)
         (succeed (apply-primitive-procedure procedure arguments)
                  fail))
        ((compound-procedure? procedure)
         ((procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))
          succeed
          fail))
        (else
         (error
          "Неизвестный тип процедуры -- EXECUTE-APPLICATION" procedure))))

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

(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

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

(define (if-alternative exp)
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

(define (require? exp)
  (tagged-list? exp 'require))

(define (require-predicate exp)
  (cadr exp))

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

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

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

(define (let->combination exp)
  (cons (make-lambda (let-var-names exp)
                (let-body exp)) (let-var-values exp)))

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
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

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

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'list list)
        (list 'null? null?)
        (list '+ +)
        (list 'not not)
        (list 'eq? eq?)))

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

(define input-prompt ";;; Ввод Amb-Eval:")
(define output-prompt ";;; Значение Amb-Eval:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Начало новой задачи")
            (ambeval input
                     the-global-environment
                     ;; успех ambeval
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; неудача ambeval
                     (lambda ()
                       (announce-output ";;; Нет больше значений")
                       (user-print input)
                       (driver-loop)))))))
 (internal-loop
  (lambda ()
    (newline)
    (display ";;; Задача не задана")
    (driver-loop))))

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

