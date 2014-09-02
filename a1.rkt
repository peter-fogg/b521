#lang racket
;; Peter Fogg

;; 1
(define countdown
  (lambda (n)
    (cond
     ((zero? n) '(0))
     (else (cons n (countdown (sub1 n)))))))

;; 2
(define insertR
  (lambda (s r l)
    (cond
     ((null? l) '())
     ((eqv? (car l) s) (cons s (cons r (insertR s r (cdr l)))))
     (else (cons (car l) (insertR s r (cdr l)))))))

;; 3
(define remv-1st
  (lambda (s l)
    (cond
     ((null? l) '())
     ((eqv? (car l) s) (cdr l))
     (else (cons (car l) (remv-1st s (cdr l)))))))

;; 4
(define occurs-?s
  (lambda (l)
    (cond
     ((null? l) 0)
     ((eqv? (car l) '?) (add1 (occurs-?s (cdr l))))
     (else (occurs-?s (cdr l))))))

;; 5
(define filter
  (lambda (p l)
    (cond
     ((null? l) '())
     ((p (car l)) (cons (car l) (filter p (cdr l))))
     (else (filter p (cdr l))))))

;; 6
(define zip
  (lambda (l1 l2)
    (cond
     ((or (null? l1) (null? l2)) '()) ;; just in case the lists aren't of equal length
     (else (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))))

;; 7
(define map
  (lambda (f l)
    (cond
     ((null? l) '())
     (else (cons (f (car l)) (map f (cdr l)))))))

;; 8
(define append
  (lambda (l1 l2)
    (cond
     ((null? l1) l2)
     (else (cons (car l1) (append (cdr l1) l2))))))

;; 8
(define reverse
  (lambda (l)
    (cond
     ((null? l) '())
     (else (append (reverse (cdr l)) (cons (car l) '()))))))

;; 10
(define fact
  (lambda (n)
    (cond
     ((zero? n) 1)
     (else (* n (fact (sub1 n)))))))

;; 11
(define member-?*
  (lambda (l)
    (cond
     ((null? l) #f)
     ((pair? (car l)) (or (member-?* (car l)) (member-?* (cdr l))))
     ((eqv? (car l) '?) #t)
     (else (member-?* (cdr l))))))

;; 12
(define fib
  (lambda (n)
    (cond
     ((zero? n) 0)
     ((zero? (sub1 n)) 1)
     (else (+ (fib (sub1 n)) (fib (- n 2)))))))

;; 13
;; > (equal? '((w x) y (z)) '((w . (x . ())) . (y . ((z . ()) . ()))))
;; #t

;; 14
(define binary->natural
  (lambda (l)
    (cond
     ((null? l) 0)
     ((zero? (car l)) (* 2 (binary->natural (cdr l))))
     (else (add1 (* 2 (binary->natural (cdr l))))))))

;; 15
(define minus
  (lambda (n1 n2)
    (cond
     ((zero? n2) n1)
     (else (sub1 (minus n1 (sub1 n2)))))))

;; 16
(define div
  (lambda (n1 n2)
    (cond
     ((zero? (sub1 n2)) n1)
     ((eqv? n1 n2) 1)
     (else (add1 (div (- n1 n2) n2))))))

;; 17

(define insertR-fr
  (lambda (s r l)
    (foldr (lambda (x acc)
             (cond
              ((eqv? s x) (cons x (cons r acc)))
              (else (cons x acc))))
           '() l)))

(define occurs-?s-fr
  (lambda (l)
    (foldr (lambda (x acc)
             (cond ((eqv? x '?) (add1 acc))
                   (else acc)))
           0 l)))

(define filter-fr
  (lambda (p l)
    (foldr (lambda (x acc)
             (cond ((p x) (cons x acc))
                   (else acc)))
           '() l)))

(define zip-fr
  (lambda (l1 l2)
    (foldr (lambda (x y acc)
             (cons (cons x y) acc))
           '() l1 l2)))

(define map-fr
  (lambda (f l)
    (foldr (lambda (x acc) (cons (f x) acc)) '() l)))

(define append-fr
  (lambda (l1 l2)
    (foldr cons l2 l1)))

(define reverse-fr
  (lambda (l)
    (foldr (lambda (x acc) (append acc (cons x '()))) '() l)))

(define binary->natural-fr
  (lambda (l)
    (foldr (lambda (x acc)
             (cond
              ((zero? x) (* 2 acc))
              (else (add1 (* 2 acc)))))
           0 l)))

;; 18
(define fact-acc  ;; This should probably be written as the body of the
  (lambda (n acc) ;; actual `fact` function, hidden inside a letrec, but
    (cond         ;; the assignment states it should take two arguments
     ((zero? n) acc)
     (else (fact-acc (sub1 n) (* n acc))))))

;; 19
(define power
  (lambda (x n)
    (cond
     ((zero? n) 1)
     ((odd? n) (* x (power x (sub1 n))))
     (else (let ((p (power x (/ n 2))))
             (* p p))))))

;; 20
(define natural->binary
  (lambda (n)
    (cond
     ((zero? n) '())
     ((even? n) (cons 0 (natural->binary (/ n 2))))
     (else (cons 1 (natural->binary (/ (sub1 n) 2)))))))

;; 21
(define base
  (lambda (x)
    (error 'error "Invalid value ~s~n" x)))
 
(define odd-case
  (lambda (recur)
    (lambda (x)
      (cond 
        ((odd? x) (collatz (add1 (* x 3)))) 
        (else (recur x))))))
 
(define even-case
  (lambda (recur)
    (lambda (x)
      (cond 
        ((even? x) (collatz (/ x 2))) 
        (else (recur x))))))
 
(define one-case
 (lambda (recur)
   (lambda (x)
     (cond
       ((zero? (sub1 x)) 1)
       (else (recur x))))))

(define collatz
  (lambda (n)
    ((one-case (odd-case (even-case base))) n)))

;; 22

;; Basically the same quine... but it works.
(define quine
  ((lambda (x) `(,x ',x))
   '(lambda (x) `(,x ',x))))
