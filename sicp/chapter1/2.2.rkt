#lang sicp

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25 36 49))

;(define (length items)
;  (if (null? items)
;      0
;      (+ 1 (length (cdr items)))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (push x items)(append items (list x)))

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

(define (reverse items)
  (define (reverse-iter list1 list2)
    (if (null? list1)
        list2
        (reverse-iter (cdr list1) (cons (car list1) list2))))
  (reverse-iter items (list)))



(define (count-change amount) (cc amount 5))

(define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else (+ (cc amount (except-first-denomination coin-values))
                   (cc (- amount
                          (first-denomination coin-values)) coin-values)))))

(define (first-denomination coin-values) (car coin-values))

(define (except-first-denomination coin-values) (cdr coin-values))

(define (no-more? coin-values) (null? coin-values))

(define us-coins (list 50 25 10 5 1))

(define reverse-us-coins (list 1 5 10 25 50))

(define (filter predicate items)
    (cond ((null? items) nil)
          ((predicate (car items))
           (cons (car items) (filter predicate (cdr items))))
          (else (filter predicate (cdr items)))))

(define (same-parity . l)
  (filter (lambda (x) (= (remainder (car l) 2) (remainder x 2))) l))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map proc (cdr items)))))

(define (for-each proc items)
  (if (null? items)
      (newline)
      ((lambda () (proc (car items)) (for-each proc (cdr items))))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))


(define (deep-reverse items)
  (define (deep-reverse-iter source result)
    (define (reverse-it item)
      (if (list? item)
          (deep-reverse item)
          item))
    (if (null? source)
        result
        (deep-reverse-iter (cdr source)
                           (cons (reverse-it (car source)) result))))
  (deep-reverse-iter items (list)))

(define x (list (list 1 2) (list 3 4)))

(define (fringe items)
  (define (fringe-iter source result)
    (define (fringe-it x)
      (if (list? x)
          (fringe x)
          (list x)))
    (if (null? source)
        result
        (fringe-iter (cdr source) (append result (fringe-it (car source))))))
  (fringe-iter items (list)))


(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor)))))

(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-map sub-tree factor)
             (* sub-tree factor)))
       tree))

(define (square x) (* x x))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

(define y (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (ss) (cons (car s) ss)) rest)))))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (fib n)
  (define (fib-inner prev next count)
    (if (< count n)
        (fib-inner next (+ prev next) (+ 1 count))
        next))
  (fib-inner 0 1 0))

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))
 ; (define (next k)
 ;   (if (> k n)
 ;       nil
 ;       (let ((f (fib k)))
 ;         (if (even? f)
 ;             (cons f (next (+ k 1)))
 ;             (next (+ k 1))))))
 ; (next 0))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

(define (map-s p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append-s seq1 seq2)
  (accumulate cons seq2 seq1))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (find-position x seq)
  (define (find-iter count x s)
    (cond ((null? s) nil)
          ((= (car s) x) count)
          (else (find-iter (+ count 1) x (cdr s)))))
  (find-iter 0 x seq))

(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
     0
     coefficient-sequence))


(define (count-leaves-with-acc x)
  (accumulate +
              0
              (map (lambda (el)
                     (if (null? el)
                         1
                         (count-leaves el))) x)))
;  (cond ((null? x) 0)
;        ((not (pair? x)) 1)
;        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

(define z (list '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12)))

(define (reverse-r seq)
  (accumulate (lambda (x y)
                (append y (list x))) nil seq))

(define (reverse-l seq)
  (fold-left (lambda (x y)
               (cons y x)) nil seq))

(define (build-pairs n)
                 (accumulate append
                             nil
                             (map (lambda (i)
                                    (map (lambda (j)
                                           (list i j))
                                         (enumerate-interval 1 (- i 1))))
                                  (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (prime? n (- times 1)))
        (else false)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cdr pair))))

(define (make-pair-sum p)
  (list (car p) (cdr p) (+ (car p) (cdr p))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))