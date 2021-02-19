#lang sicp

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Недостаточно денег на счёте"))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account balance pwd)
  (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Недостаточно денег"))
  (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch password message)
      (cond ((eq? message 'withdraw) withdraw)
            ((eq? message 'deposit) deposit)
            ((eq? message 'serializer) balance-serializer)
            (else (error "Неизвестный вызов -- MAKE-ACCOUNT"
                         message))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange)) account1 account2)))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acuire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (clear! cell)
  (set-car! cell false))

(define (make-semafor n)
  (let ((mutex-inner (make-mutex))
        (mutex-outer (make-mutex))
        (counter n))
    (define (the-semafor m)
      (cond ((eq? m 'acquire)
             (if (< counter n)
                 (begin
                   (mutex-inner 'acquire)
                   (set! counter (+ counter 1))
                   (mutex-inner 'release))
                 (mutex-outer 'acquire)
                 ))
            ((eq? m 'release)
             (begin (mutex-inner 'acquire)
                    (set! counter 0)
                    (mutex-inner 'release)
                    (mutex-outer 'release)
                    ))))
    the-semafor))








