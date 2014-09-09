#lang racket
;; Peter Fogg

(require C311/pmatch)

;; 1
(define list-ref
  (lambda (ls n)
    (letrec ((nth-cdr (lambda (n)
                        (cond
                         ((zero? n) ls)
                         (else (cdr (nth-cdr (sub1 n))))))))
      (car (nth-cdr n)))))

;; 2
(define union
  (lambda (l1 l2)
    (letrec ((go (lambda (l)
                    (cond
                     ((null? l) l2)
                     ((memv (car l) l2) (go (cdr l)))
                     (else (cons (car l) (go (cdr l))))))))
      (go l1))))

;; 3
(define extend
  (lambda (x pred)
    (lambda (y)
      (or (eqv? x y) (pred y)))))

;; 4
(define walk-symbol
  (lambda (x s)
    (letrec ((go (lambda (al)
                   (cond
                    ((null? al) x)
                    ((eqv? (caar al) x)
                     (let ((val (cdar al)))
                      (cond
                       ((symbol? val) (walk-symbol val s))
                       (else val))))
                    (else (go (cdr al)))))))
      (go s))))

;; 5
(define lambda->lumbda
  (lambda (e)
    (pmatch e
      (`,x (guard (symbol? x)) x)
      (`(lambda (,x) ,body) `(lumbda (,x) ,(lambda->lumbda body)))
      (`(,rator ,rand) `(,(lambda->lumbda rator) ,(lambda->lumbda rand))))))

;; 6
(define vars
  (lambda (e)
    (pmatch e
      (`,x (guard (symbol? x)) (list x))
      (`(lambda (,x) ,body) (vars body))
      (`(,rator ,rand) (append (vars rator) (vars rand))))))

;; 7
(define unique-vars
  (lambda (e)
    (pmatch e
      (`,x (guard (symbol? x)) (list x))
      (`(lambda (,x) ,body) (unique-vars body))
      (`(,rator ,rand) (union (unique-vars rator) (unique-vars rand))))))

;; 8
(define var-occurs-free?
  (lambda (x exp)
    (pmatch exp
      (`,y (guard (symbol? y)) (eqv? y x))
      (`(lambda (,y) ,body) (and (var-occurs-free? x body) (not (eqv? y x))))
      (`(,rator ,rand) (or (var-occurs-free? x rator)
                           (var-occurs-free? x rand))))))

;; 9
(define var-occurs-bound?
  (lambda (x exp)
    (pmatch exp
      (`,y (guard (symbol? y)) #f)
      (`(lambda (,y) ,body) (or (var-occurs-bound? x body)
                                (and (eqv? y x) (var-occurs-free? x body))))
      (`(,rator ,rand) (or (var-occurs-bound? x rator)
                           (var-occurs-bound? x rand))))))

;; 10
(define unique-free-vars
  (lambda (exp)
    (pmatch exp
      (`,x (guard (symbol? x)) (list x))
      (`(lambda (,x) ,body) (let ((ufv (unique-free-vars body)))
                              (if (memv x ufv)
                                  (remove x ufv)
                                  ufv)))
      (`(,rator ,rand) (union (unique-free-vars rator)
                              (unique-free-vars rand))))))

;; 11
(define unique-bound-vars
  (lambda (exp)
    (pmatch exp
      (`,x (guard (symbol? x)) '())
      (`(lambda (,x) ,body) (let ((ubv (unique-bound-vars body)))
                              (if (memv x (unique-free-vars body))
                                  (union (list x) ubv)
                                  ubv)))
      (`(,rator ,rand) (union (unique-bound-vars rator)
                              (unique-bound-vars rand))))))

;; 12
(define lex
  (lambda (exp env)
    (letrec ((map-alist (lambda (f al)
                          (cond
                           ((null? al) '())
                           (else (cons (cons (caar al) (f (cdar al)))
                                       (map-alist f (cdr al))))))))
      (pmatch exp
        (`,x (guard (symbol? x)) (cond
                                  ((eqv? x (walk-symbol x env))
                                   `(free-var ,x))
                                  (else
                                   `(var ,(walk-symbol x env)))))
        (`(lambda (,y) ,body) `(lambda ,(lex body (cons (cons y 0)
                                                        (map-alist add1 env)))))
        (`(,rator ,rand) `(,(lex rator env) ,(lex rand env)))))))

;; 13
(define walk*
  (lambda (v s)
    (let ((v (cond ((symbol? v) (walk-symbol v s))
                   (else v))))
      (cond
       ((pair? v) (cons (walk* (car v) s)
                        (walk* (cdr v) s)))
       (else v)))))

(define unify
  (lambda (u v s)
    (letrec ((replace (lambda (l)
                        (cond
                         ((not (list? l)) l)
                         ((null? l) '())
                         ((symbol? (car l)) (cons (walk* (car l) s)
                                                  (replace (cdr l))))
                         ((pair? (car l)) (cons (walk* (car l) s)
                                                (replace (cdr l))))
                         (else (cons (car l) (replace (cdr l))))))))
      (cond
       ((equal? u v) s)
       ((symbol? u) (cons (cons u v) s))
       ((symbol? v) (cons (cons v u) s))
       ((and (pair? u) (pair? v))
        (let ((ru (replace u))
              (rv (replace v)))
          (cond ((equal? ru rv) s)
                (else (let ((r (unify (car ru) (car rv) s)))
                        (and r (unify (cdr ru) (cdr rv) r)))))))
       (else #f)))))

;; 14
(define walk-symbol-update
  (lambda (x s)
    (letrec ((go (lambda (al)
                   (cond
                    ((null? al) x)
                    ((eqv? (caar al) x)
                     (let* ((b (cdar al))
                           (val (unbox b)))
                       (cond
                        ((symbol? val)
                         (let ((new-val (walk-symbol-update val s)))
                           (set-box! b new-val)
                           new-val))
                        (else val))))
                    (else (go (cdr al)))))))
      (go s))))

;; 15
(define var-occurs-both?
  (lambda (x exp)
   (letrec ((go (lambda (e)
                  (pmatch e
                    (`,y (guard (symbol? y)) (and (eqv? y x) (list 'free)))
                    (`(lambda (,y) ,body) (let ((r (go body)))
                                            (cond
                                             ((not r) #f)
                                             ((and (eqv? y x) (memv 'free r))
                                              (list 'bound))
                                             (else r))))
                    (`(,rator ,rand) (let ((r (go rator))
                                           (d (go rand)))
                                       (and r d (union r d))))))))
     (let ((result (go exp)))
       (if (and result
                (memv 'bound result)
                (memv 'free result))
           #t
           #f)))))
