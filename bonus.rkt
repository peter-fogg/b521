#lang racket

(require c311/let-pair)

;; 1

(define filter-sps
  (lambda (f l s)
    (if (null? l)
        `(() . ,s)
        (let-pair ((r . s) (filter-sps f (cdr l) s))
          (if (f (car l))
              `(,(cons (car l) r) . ,s)
              `(,r . ,(cons (car l) s)))))))

;; 2

(define filter*-sps
  (lambda (f l s)
    (cond
     ((null? l) `(() . ,s))
     ((pair? (car l)) (let-pair ((ra . s^) (filter*-sps f (car l) s))
                        (let-pair ((rd . s^^) (filter*-sps f (cdr l) s))
                          `(,(cons ra rd) . ,(cons s^ s^^)))))
     ((null? (car l)) `(() . ,(cons '() s)))
     ((f (car l)) (let-pair ((r . s^) (filter*-sps f (cdr l) s))
                    `(,(cons (car l) r) . ,s^)))
     (else (let-pair ((r . s^) (filter*-sps f (cdr l) s))
             `(,r . ,(cons (car l) s^)))))))

;; 3

(define fib-sps
  (lambda (n s)
    (cond
     ((assv n s) => (lambda (p) `(,(cdr p) . ,s)))
     ((< n 2) `(,n . ((,n . ,n))))
     (else (let-pair ((n1 . s^) (fib-sps (sub1 n) s))
             (let-pair ((n2 . s^^) (fib-sps (- n 2) s^))
               (let ((r (+ n1 n2)))
                 `(,r . ,(cons `(,n . ,r) s^^)))))))))

;; 4

(define-syntax and*
  (syntax-rules ()
    ((_) #t)
    ((_ x) (if x x #f))
    ((_ x y ...) (if x (and* y ...)
                     x))))

;; 5

(define-syntax cons*
  (syntax-rules ()
    ((_) (raise-syntax-error 'cons* "Incorrect argument count to cons*"))
    ((_ x) x)
    ((_ x y ...) (cons x (cons* y ...)))))

;; 6

(define-syntax macro-list
  (syntax-rules ()
    ((_) '())
    ((_ x y ...) (cons x (macro-list y ...)))))

;; 7

(define-syntax mcond
  (syntax-rules (=> else)
    ((_) (void))
    ((_ (else res)) res)
    ((_ (clause) rest ...) (if clause clause
                               (mcond rest ...)))
    ((_ (test => f) rest ...) (let ((r test))
                                (if r (f r)
                                    (mcond rest ...))))
    ((_ (test res) rest ...) (if test res
                                 (mcond rest ...)))))

;; 8

(define-syntax macro-map
  (syntax-rules ()
    ((_ m (_ . ((a . d) . _))) (cons (m a) (macro-map m (list d))))
    ((_ _ _) '())))

;; Just Dessert

(define-syntax condre
  (syntax-rules (else => let let* letrec)
    ((_) (void))
    ((_ (letrec ((var val) ...) rest ...))
     (letrec ((var val) ...)
       (condre rest ...)))
    ((_ (let ((var val) ...) rest ...))
     (let ((var val) ...)
       (condre rest ...)))
    ((_ (let* ((var val) ...) rest ...))
     (let* ((var val) ...)
       (condre rest ...)))
    ((_ (else res)) res)
    ((_ (test => f ...) rest ...) (let ((r test))
                                    (if r
                                        (begin
                                          (f r) ...)
                                        (condre rest ...))))
    ((_ (test res) rest ...) (if test res
                                 (condre rest ...)))))

(condre
 ((null? '(1)) 'asplode)
 (letrec ((a 5)
          (b (add1 a))
          (f (lambda (x) (if (zero? x) 1 (* x (f (sub1 x)))))))
   (a => display f)))
