#lang racket

;; Peter Fogg

(require c311/pmatch)

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
                (error 'empty-k "You can only invoke the empty continuation once")
                    (begin (set! once-only #t) v))))))

;; 1

(define last-non-zero
  (lambda (ls)
    (call/cc
     (lambda (k)
       (letrec ((lnz (lambda (l)
                       (cond
                        ((null? l) '())
                        ((zero? (car l)) (k (lnz (cdr l))))
                        (else (cons (car l) (lnz (cdr l))))))))
         (lnz ls))))))

;; 2

(define my-*
  (lambda (n m)
    (* n m)))

(define mult/cc
  (lambda (n*)
    (call/cc
     (lambda (k)
       (letrec ((m/cc (lambda (n*)
                        (cond
                         ((null? n*) 1)
                         ((zero? (car n*)) (k 0))
                         (else (my-* (car n*) (m/cc (cdr n*))))))))
         (m/cc n*))))))

;; 3

(define times-cps
  (lambda (ls k)
    (cond
     ((null? ls) (k 1))
     ((zero? (car ls)) (k 0))
     (else (times-cps (cdr ls) (lambda (n)
                                 (k (* n (car ls)))))))))

;; 4

(define times-cps-shortcut
  (lambda (ls k)
    (cond
     ((null? ls) (k 1))
     ((zero? (car ls)) 0)
     (else (times-cps-shortcut (cdr ls) (lambda (n)
                                          (k (* n (car ls)))))))))

;; 5

(define plus-cps
  (lambda (m k)
    (k (lambda (n k^)
         (k^ (+ m n))))))

;; 6

(define count-syms*-cps
  (lambda (ls k)
    (cond
     ((null? ls) (k 0))
     ((pair? (car ls)) (count-syms*-cps (car ls)
                                        (lambda (n)
                                          (count-syms*-cps (cdr ls)
                                                           (lambda (m)
                                                             (k (+ m n)))))))
     ((symbol? (car ls)) (count-syms*-cps (cdr ls) (lambda (n) (k (add1 n)))))
     (else (count-syms*-cps (cdr ls) k)))))

;; 7

(define cons-cell-count-cps
  (lambda (ls k)
    (cond
     ((pair? ls) (cons-cell-count-cps (car ls)
                                      (lambda (n)
                                        (cons-cell-count-cps (cdr ls)
                                                             (lambda (m)
                                                               (k (add1 (+ m n))))))) )
     (else (k 0)))))

;; 8

(define walk
  (lambda (v ls)
    (cond
     ((symbol? v) (let ((p (assq v ls)))
                    (cond
                     (p (walk (cdr p) ls))
                     (else v))))
     (else v))))

(define walk-cps
  (lambda (v ls k)
    (cond
     ((symbol? v) (let ((p (assq v ls)))
                    (cond
                     (p (walk-cps (cdr p) ls k))
                     (else (k v)))))
     (else (k v)))))

;; 9

(define ack-cps
  (lambda (m n k)
    (cond
     ((zero? m) (k (add1 n)))
     ((zero? n) (ack-cps (sub1 m) 1 k))
     (else (ack-cps m (sub1 n) (lambda (n^)
                             (ack-cps (sub1 m) n^ k)))))))

;; 10

(define fib-cps
  (lambda (n k)
    ((lambda (fib-cps k)
       (fib-cps fib-cps n k))
     (lambda (fib-cps n k)
       (cond
        ((zero? n) (k 0))
        ((= 1 n) (k 1))
        (else (fib-cps fib-cps (sub1 n)
                       (lambda (n^)
                         (fib-cps fib-cps (- n 2)
                                  (lambda (n^^)
                                    (k (+ n^ n^^))))))))) k)))

;; 11

(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h k)
       (h h (lambda (hh)
              (hh seed '() k))))
     (lambda (h k)
       (k (lambda (seed ans k)
            (p seed (lambda (b)
                      (if b
                          (k ans)
                          (h h (lambda (hh)
                                 (g seed (lambda (gs)
                                           (f seed (lambda (fs)
                                                     (hh gs (cons fs ans) k)))))))))))))
     k)))

(define null?-cps
  (lambda (x k)
    (k (null? x))))

(define car-cps
  (lambda (x k)
    (k (car x))))

(define cdr-cps
  (lambda (x k)
    (k (cdr x))))

;; 12

(define empty-s
  (lambda ()
    '()))

(define extend-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

(define unify
  (lambda (v w s)
    (let ((v (walk v s)))
      (let ((w (walk w s)))
        (cond
         ((eqv? v w) s)
         ((symbol? v) (extend-s v w s))
         ((symbol? w) (extend-s w v s))
         ((and (pair? v) (pair? w))
          (let ((s (unify (car v) (car w) s)))
            (cond
             (s (unify (cdr v) (cdr w) s))
             (else #f))))
         ((equal? v w) s)
         (else #f))))))

(define unify-cps
  (lambda (v w s k)
    (walk-cps v s (lambda (v)
                    (walk-cps w s (lambda (w)
                                    (cond
                                     ((eqv? v w) (k s))
                                     ((symbol? v) (k (extend-s v w s)))
                                     ((symbol? w) (k (extend-s w v s)))
                                     ((and (pair? v) (pair? w))
                                      (unify-cps (car v) (car w) s
                                                 (lambda (s)
                                                   (cond
                                                    (s (unify-cps (cdr v) (cdr w) s k))
                                                    (else (k #f))))))
                                     ((equal? v w) (k s))
                                     (else (k #f)))))))))

;; 13

(define M-cps
  (lambda (f k)
    (k (lambda (ls k)
         (cond
          ((null? ls) (k '()))
          (else (f (car ls) (lambda (a)
                              (M-cps f (lambda (f^)
                                         (cons a (f^ (cdr ls) k))))))))))))

;; 14

(define use-of-M-cps
  ((M-cps (lambda (n k) (k (add1 n))) (empty-k))
   '(1 2 3 4 5) (empty-k)))

;; 15

(define strange-cps
  (lambda (x k)
    ((lambda (g k) (lambda (x k) (g g k)))
     (lambda (g k) (lambda (x k) (g g k)))
     k)))

;; 16

(define use-of-strange-cps
  (let ((strange^ (strange-cps 5 (lambda (f)
                                   (f 6 (lambda (g)
                                          (g 7 (empty-k))))))))
    (strange^ 8 (lambda (f)
                  (f 9 (lambda (g)
                         (g 10 (empty-k))))))))

;; 17

(define why-cps
  (lambda (f k)
    ((lambda (g k)
       (f (lambda (x k) (g g (lambda (h) (h x k)))) k))
     (lambda (g k)
       (f (lambda (x k) (g g (lambda (h) (h x k)))) k))
     k)))

;; 18

(define why-cps-cps
  (lambda (f c k)
    ((lambda (g c k)
       (f (lambda (x c k)
            (g g (lambda (h k) (h x c k)) k)) c k))
     (lambda (g c k)
       (f (lambda (x c k)
            (g g (lambda (h k) (h x c k)) k)) c k))
     c
     k)))

(define test
  (lambda (fact-cps k)
    (k (lambda (n k)
         (if (zero? n)
             (k 1)
             (fact-cps (sub1 n) (lambda (n^)
                                  (k (* n n^)))))))))

(define test-cps
  (lambda (fact-cps c k)
    (c (lambda (n c k)
         (if (zero? n)
             (c 1 k)
             (fact-cps (sub1 n) (lambda (n^ k)
                                  (c (* n n^) k))
                       k))) k)))

((why-cps test (empty-k)) 5 (empty-k))
((why-cps-cps test-cps (lambda (x k) (k x)) (empty-k)) 5 (lambda (x k) (k x)) (empty-k))
