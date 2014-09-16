#lang racket
;; Peter Fogg

(require C311/pmatch)

;; Part 1

;; Commenting this out because it's extended later to include binary functions

;; Representionally dependent:
;; (define value-of
;;   (lambda (exp env)
;;     (pmatch exp
;;       (`,n (guard (number? n)) n)
;;       (`,b (guard (boolean? b)) b)
;;       (`,x (guard (symbol? x)) (env x))
;;       (`(zero? ,e) (if (zero? (value-of e env))
;;                        #t
;;                        #f))
;;       (`(sub1 ,n) (sub1 (value-of n env)))
;;       (`(* ,n1 ,n2) (* (value-of n1 env) (value-of n2 env)))
;;       (`(if ,test-exp ,then-exp ,else-exp) (if (value-of test-exp env)
;;                                                (value-of then-exp env)
;;                                                (value-of else-exp env)))
;;       (`(let ((,var ,value)) ,body) (value-of body (lambda (x) (if (eqv? var x) (value-of value env) (env x)))))
;;       (`(lambda (,x) ,body) (lambda (a) (value-of body (lambda (y) (if (eqv? y x) a (env y))))))
;;       (`(,rator ,rand) ((value-of rator env) (value-of rand env))))))

;; Representationally independent (but using functions):
(define empty-env-fn
  (lambda ()
   (lambda (_) (error "unbound variable reference!"))))

(define extend-env-fn
  (lambda (x a env)
    (lambda (y)
      (if (eqv? y x)
          a
          (env y)))))

(define apply-env-fn
  (lambda (env x)
    (env x)))

(define value-of-fn
  (lambda (exp env)
    (pmatch exp
      (`,n (guard (number? n)) n)
      (`,b (guard (boolean? b)) b)
      (`,x (guard (symbol? x)) (apply-env-fn env x))
      (`(zero? ,e) (if (zero? (value-of-fn e env))
                       #t
                       #f))
      (`(sub1 ,n) (sub1 (value-of-fn n env)))
      (`(* ,n1 ,n2) (* (value-of-fn n1 env) (value-of-fn n2 env)))
      (`(if ,test-exp ,then-exp ,else-exp) (if (value-of-fn test-exp env)
                                               (value-of-fn then-exp env)
                                               (value-of-fn else-exp env)))
      (`(let ((,var ,value)) ,body) (value-of-fn body (extend-env-fn var (value-of-fn value env) env)))
      (`(lambda (,x) ,body) (lambda (a) (value-of-fn body (extend-env-fn x a env))))
      (`(,rator ,rand) ((value-of-fn rator env) (value-of-fn rand env))))))

;; Representationally independent (but using data structures):
(define empty-env-ds
  (lambda ()
    '(empty-env-ds)))

(define extend-env-ds
  (lambda (x a env)
    `(extend-env-ds ,x ,a ,env)))

(define apply-env-ds
  (lambda (env x)
    (pmatch env
      (`(empty-env-ds) (error "unbound variable reference!"))
      (`(extend-env-ds ,y ,a ,rest) (if (eqv? y x)
                                        a
                                        (apply-env-ds rest x))))))

(define value-of-ds
  (lambda (exp env)
    (pmatch exp
      (`,n (guard (number? n)) n)
      (`,b (guard (boolean? b)) b)
      (`,x (guard (symbol? x)) (apply-env-ds env x))
      (`(zero? ,e) (if (zero? (value-of-ds e env))
                       #t
                       #f))
      (`(sub1 ,n) (sub1 (value-of-ds n env)))
      (`(* ,n1 ,n2) (* (value-of-ds n1 env) (value-of-ds n2 env)))
      (`(if ,test-exp ,then-exp ,else-exp) (if (value-of-ds test-exp env)
                                               (value-of-ds then-exp env)
                                               (value-of-ds else-exp env)))
      (`(let ((,var ,value)) ,body) (value-of-ds body (extend-env-ds var (value-of-ds value env) env)))
      (`(lambda (,x) ,body) (lambda (a) (value-of-ds body (extend-env-ds x a env))))
      (`(,rator ,rand) ((value-of-ds rator env) (value-of-ds rand env))))))

;; Part 2

;; Commenting this out because it's extended later to include begin2 and set!

;; (define value-of
;;   (lambda (exp env)
;;     (pmatch exp
;;       (`,n (guard (number? n)) n)
;;       (`,b (guard (boolean? b)) b)
;;       (`,x (guard (symbol? x)) (env x))
;;       (`(zero? ,e) (if (zero? (value-of e env))
;;                        #t
;;                        #f))
;;       (`(sub1 ,n) (sub1 (value-of n env)))
;;       (`(* ,n1 ,n2) (* (value-of n1 env) (value-of n2 env)))
;;       (`(if ,test-exp ,then-exp ,else-exp) (if (value-of test-exp env)
;;                                                (value-of then-exp env)
;;                                                (value-of else-exp env)))
;;       (`(let ((,var ,value)) ,body) (value-of body (lambda (x) (if (eqv? var x) (value-of value env) (env x)))))
;;       (`(lambda (,x) ,body) (lambda (a) (value-of body (lambda (y) (if (eqv? y x) a (env y))))))
;;       (`(lambda (,x ,y) ,body) (lambda (a b) (value-of body (lambda (p)
;;                                                               (if (eqv? p x)
;;                                                                   a
;;                                                                   ((lambda (q)
;;                                                                      (if (eqv? q y)
;;                                                                          b
;;                                                                          (env q))) p))))))
;;       (`(,rator ,rand) ((value-of rator env) (value-of rand env)))
;;       (`(,rator ,rand1 ,rand2) ((value-of rator env) (value-of rand1 env) (value-of rand2 env))))))

;; Part 3

(define empty-env
  (lambda ()
    (lambda (_)
      (error "unbound variable reference!"))))

;; (define fo-eulav
;;   (lambda (exp env)
;;     (pmatch exp
;;       (`,n (guard (number? n)) n)
;;       (`,b (guard (boolean? b)) b)
;;       (`,x (guard (symbol? x)) (env x))
;;       (`(,e ?orez) (if (zero? (fo-eulav e env)) #t #f))
;;       (`(,n 1bus) (sub1 (fo-eulav n env)))
;;       (`(,n2 ,n1 *) (* (fo-eulav n1 env) (fo-eulav n2 env)))
;;       (`(,else-exp ,then-exp ,test-exp fi) (if (fo-eulav test-exp env)
;;                                                (fo-eulav then-exp env)
;;                                                (fo-eulav else-exp env)))
;;       (`(,body (,x) adbmal) (lambda (a) (fo-eulav body (lambda (y) (if (eqv? y x) a (env y))))))
;;       (`(,rand ,rator) ((fo-eulav rator env) (fo-eulav rand env))))))

(define reverse-exp
  (lambda (exp)
    (cond
     ((number? exp) exp)
     ((symbol? exp) (string->symbol (list->string (reverse (string->list (symbol->string exp))))))
     (else (map reverse-exp (reverse exp))))))

(define fo-eulav
  (lambda (exp env)
    (value-of (reverse-exp exp) env)))

;; Brainteasers

(define value-of
  (lambda (exp env)
    (pmatch exp
      (`,n (guard (number? n)) n)
      (`,b (guard (boolean? b)) b)
      (`,x (guard (symbol? x)) (unbox (env x)))
      (`(zero? ,e) (if (zero? (value-of e env))
                       #t
                       #f))
      (`(sub1 ,n) (sub1 (value-of n env)))
      (`(* ,n1 ,n2) (* (value-of n1 env) (value-of n2 env)))
      (`(if ,test-exp ,then-exp ,else-exp) (if (value-of test-exp env)
                                               (value-of then-exp env)
                                               (value-of else-exp env)))
      (`(let ((,var ,value)) ,body) (value-of body (let ((b (box (value-of value env))))
                                                     (lambda (x) (if (eqv? var x) b (env x))))))
      (`(begin2 ,exp1 ,exp2) (begin
                               (value-of exp1 env)
                               (value-of exp2 env)))
      (`(set! ,var ,value) (set-box! (env var) (value-of value env)))
      (`(lambda (,x) ,body) (lambda (a) (value-of body (let ((b (box a)))
                                                         (lambda (y) (if (eqv? y x) b (env y)))))))
      (`(lambda (,x ,y) ,body) (lambda (a b) (value-of body (let ((b1 (box a))
                                                                  (b2 (box b)))
                                                              (lambda (p)
                                                                (if (eqv? p x)
                                                                    b1
                                                                    ((lambda (q)
                                                                       (if (eqv? q y)
                                                                           b2
                                                                           (env q))) p)))))))
      (`(,rator ,rand) ((value-of rator env) (value-of rand env)))
      (`(,rator ,rand1 ,rand2) ((value-of rator env) (value-of rand1 env) (value-of rand2 env))))))

(define value-of-lex
  (lambda (exp env)
    (pmatch exp
      (`,c (guard (or (boolean? c) (number? c))) c) 
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(zero? ,body) (zero? (value-of-lex body env)))
      (`(* ,n1 ,n2) (* (value-of-lex n1 env) (value-of-lex n2 env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))
 
(define empty-env-lex 
  (lambda () '()))

(define extend-env-lex cons)

(define apply-env-lex list-ref)

;; Just Dessert

(define c0 (lambda (f) (lambda (x) x)))
(define c5 (lambda (f) (lambda (x) (f (f (f (f (f x))))))))

(define c+
  (lambda (n m)
    (lambda (f)
      (lambda (x)
        ((n f) ((m f) x))))))

(define csub1
  (lambda (n)
    (lambda (f)
      (lambda (x)
        ((n f) x)))))

;; let's not do this one
;; (define csub1
;;   (lambda (n)
;;     (lambda (f)
;;       (let* ((used #f)
;;              (new-f (lambda (y)
;;                       (if used
;;                           (f y)
;;                           (begin (set! used #t) y)))))
;;         (lambda (x)
;;           ((n new-f) x))))))
