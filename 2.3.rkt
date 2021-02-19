#lang sicp

(define a 1)
(define b 2)

(define (memq item items)
  (cond ((null? items) false)
        ((eq? item (car items)) items)
        (else (memq item (cdr items)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;(define (equal? list1 list2)
;  (cond ((and (null? list1) (null? list2)) #t)
;        ((and (symbol? (car list1)) (symbol? (car list2)) (eq? (car list1) (car list2)))
;         (equal? (cdr list1) (cdr list2)))
;        (else #f)))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                       (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                       (multiplicand exp))))
        ((exponentation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentation (base exp)
                               (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "неизвестный тип выражения -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;(define (make-sum a1 a2)
;  (list '+ a1 a2))

;(define (make-sum a1 a2)
;  (cond ((=number? a1 0) a2)
;        ((=number? a2 0) a1)
;        ((and (number? a1) (number? a2)) (+ a1 a2))
;        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;(define (make-product m1 m2)
;  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;        ((=number? m1 1) m2)
;        ((=number? m2 1) m1)
;        ((and (number? m1) (number? m2)) (* m1 m2))
;        (else (list '* m1 m2))))

;(define (sum? x)
;  (and (pair? x) (eq? (car x) '+)))
(define (split el items)
  (define (split-inner dist source)
    (cond ((null? source) (list))
          ((eq? (car source) el) (list dist (cdr source)))
          (else (split-inner (append dist (list (car source)))
                             (cdr source)))))
  (split-inner (list) items))

(define (omit-parenthesis x)
  (if (null? (cdr x))
      (car x)
      x))

(define (sum? x)
  (and (pair? x)
       (not (null? (split '+ x)))))

(define (addend s) (omit-parenthesis (car (split '+ s))))
(define (augend s) (omit-parenthesis (cadr (split '+ s))))

(define (exponentation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (cadr x))
(define (exponent x)
  (caddr x))

(define (power a b) 
  (exp (* b (log a))))

(define (make-exponentation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? base) (number? exp)) (power base exp))
        (else (list '** base exp))))

;(define (addend s)  (cadr s))
;(define (augend s) (caddr s))
;(define (augend s)
;  (let ((last (cddr s)))
;    (if (null? (cdr last))
;        (car last)
;        (accumulate make-sum 0 last))))

;(define (product? x)
;  (and (pair? x) (eq? (car x) '*)))

(define (product? x)
  (and (pair? x)
       (not (sum? x))
       (not (null? (split '* x)))))

;(define (multiplier p)
;  (cadr p))
;(define (multiplicand p)
;  (let ((last (cddr p)))
;    (if (null? (cdr last))
;        (car last)
;        (accumulate make-product 1 last))))

(define (multiplier s) (omit-parenthesis (car (split '* s))))
(define (multiplicand s) (omit-parenthesis (cadr (split '* s))))


(define (add-parenthesis predicate? e)
  (if (predicate? e)
      e
      (list e)))

(define (sum-or-product? e)
  (pair? e))

(define (make-operation op predicate? arg1 arg2)
  (append (add-parenthesis predicate? arg1)
          (list op)
          (add-parenthesis predicate? arg2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (make-operation '+ sum-or-product? a1 a2))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (make-operation '* product? m1 m2))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;(define (adjoin-set x set)
;  (if (element-of-set? x set)
;      set
;      (cons x set)))

;(define (adjoin-set x set)
;  (define (adjoin-set-inner start tail)
;    (cond ((null? tail) (append start tail '(x)))
;          ((= x (car tail)) set)
;          ((< x (car tail)) (append start '(x) tail))
;          (else (adjoin-set-inner (append start '((car tail))) (cdr tail)))))
;  (adjoin-set-inner '() set))

;(define (intersection-set set1 set2)
;  (cond ((or (null? set1) (null? set2)) '())
;        ((element-of-set? (car set1) set2)
;         (cons (car set1)
;               (intersection-set (cdr set1) set2)))
;        (else (intersection-set (cdr set1) set2))))

;(define (union-set set1 set2)
;  (cond ((null? set1) set2)
;        ((null? set2) set1)
;        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
;        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
        ((< (car set2) (car set1)) (cons (car set2) (union-set set1 (cdr set2))))
        (else (cons (car set1) (union-set (cdr set1) (cdr set2))))))

;(define (element-of-set? x set)
;  (cond ((null? set) false)
;        ((= x (car set)) true)
;        ((< x (car set)) false)
;        (else (element-of-set? x (cdr x)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              (else (intersection-set set1 (cdr set2)))))))

(define (entry tree) (car tree))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (make-tree entry left right) (list entry left right))

;(define (element-of-set? x set)
;  (cond ((null? set) false)
;        ((= x (entry set)) true)
;        ((< x (entry set))
;         (element-of-set? x (left-branch set)))
;        (else (element-of-set? x (right-branch set)))))

;(define (adjoin-set x set)
;  (cond ((null? set) (make-tree x '() '()))
;        ((= x (entry set)) set)
;        ((< x (entry set))
;         (make-tree (entry set)
;                    (adjoin-set x (left-branch set))
;                    (right-branch set)))
;        (else (make-tree (entry set)
;                         (left-branch set)
;                         (adjoin-set x (right-branch set))))))

(define test (make-tree '() '() '()))



;(define (lookup given-key set-of-records)
;  (cond ((null? set-of-records) false)
;        ((equal? given-key (key (car set-of-records)))
;         (car set-of-records))
;        (else (lookup given-key (cdr set-of-records)))))

;(define (lookup-tree given-key set-of-records)
;  (cond ((null? set-of-records) false)
;        ((= given-key (key (entry set-of-records))) (entry set-of-records))
;        ((< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))
;        (else (lookup given-key (lookup given-key (right-branch set-of-records))))))


(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-code-branch tree)
  (car tree))

(define (right-code-brach tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (in-tree? symbol tree) (element-of-set? symbol (symbols tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "плохой бит -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4) (make-code-tree (make-leaf 'B 2)
                                                  (make-code-tree (make-leaf 'D 1)
                                                                  (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; A D A B C A
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (let ((l-branch (left-branch tree))
        (r-branch (right-branch tree)))
    (cond ((not (in-tree? symbol tree)) (error "СИМВОЛ НЕ СОДЕРЖИТСЯ В ДЕРЕВЕ -- ENCODE-SYMBOL" symbol))
          ((and (leaf? l-branch) (not (leaf? r-branch))) (if (eq? (symbol-leaf l-branch) symbol)
                                (list 0)
                                (cons 1 (encode-symbol symbol r-branch))))
          ((and (leaf? r-branch) (not (leaf? l-branch))) (if (eq? (symbol-leaf r-branch) symbol)
                                     (list 1)
                                     (cons 0 (encode-symbol symbol l-branch))))
          ((and (leaf? l-branch) (leaf? r-branch)) (if (eq? (symbol-leaf l-branch) symbol)
                                                       (list 0)
                                                       (list 1)))
          (else (if (in-tree? symbol l-branch)
                    (cons 0 (encode-symbol symbol l-branch))
                    (cons 1 (encode-symbol symbol r-branch)))))
          ))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge trees)
  (if (null? (cdr trees))
      (car trees)
      (successive-merge (adjoin-set (make-code-tree (car trees) (cadr trees)) (cddr trees)))))

(define songs-tree (generate-huffman-tree (list '(A 2) '(BOOM 1) '(GET 2) '(JOB 2) '(NA 16) '(SHA 3) '(YIP 9) '(WAH 1))))

(define song (list 'GET 'A 'JOB 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'GET 'A 'JOB 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'WAH 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'SHA 'BOOM))
