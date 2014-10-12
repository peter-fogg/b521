#lang racket

;; Peter Fogg

(require c311/pmatch)

;; 1

(define binary-to-decimal-cps
  (lambda (n k)
    (cond
     ((null? n) (k 0))
     (else (binary-to-decimal-cps
            (cdr n)
            (lambda (n^)
              (k (+ (car n) (* 2 n^)))))))))

;; 2

(define rember*1-cps
  (lambda (ls k)
    (cond
     ((null? ls) (k '()))
     ((pair? (car ls))
      (rember*1-cps (car ls)
                    (lambda (car^)
                      (cond
                       ((equal? (car ls) car^) (rember*1-cps (cdr ls)
                                                             (lambda (cdr^)
                                                               (k (cons (car ls) cdr^)))))
                       (else (rember*1-cps (car ls)
                                           (lambda (car^)
                                             (k (cons car^ (cdr ls))))))))))
     ((eqv? (car ls) '?) (k (cdr ls)))
     (else (rember*1-cps (cdr ls)
                         (lambda (cdr^)
                           (k (cons (car ls) cdr^))))))))

;; Interpreter

(define empty-env
  (lambda ()
    (lambda (y)
      (error "unbound variable" y))))

(define apply-env
  (lambda (env x)
    (env x)))

(define extend-env
  (lambda (x a env)
    (lambda (y)
      (if (eqv? x y)
          a
          (env y)))))

(define closure
  (lambda (x body env)
    (lambda (a)
      (value-of body (extend-env x a env)))))

(define apply-closure
  (lambda (c a)
    (c a)))

(define value-of
  (lambda (expr env)
    (pmatch expr
      (`,n (guard (or (number? n) (boolean? n))) n)
      (`(+ ,n1 ,n2) (+ (value-of n1 env) (value-of n2 env)))
      (`(* ,n1 ,n2) (* (value-of n1 env) (value-of n2 env)))
      (`(sub1 ,n) (sub1 (value-of n env)))
      (`(zero? ,x) (zero? (value-of x env)))
      (`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env)))
      (`(capture ,k-id ,body) (call/cc (lambda (k)
                                         (value-of body (extend-env k-id k env)))))
      (`(return ,v-exp ,k-exp) ((value-of k-exp env) (value-of v-exp env)))
      (`,x (guard (symbol? x)) (apply-env env x))
      (`(lambda (,id) ,body) (closure id body env))
      (`(,rator ,rand) (apply-closure (value-of rator env) (value-of rand env))))))

(define fact-5
  '((lambda (f)
      ((f f) 5))
    (lambda (f)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))

(define capture-fun
    '(* 3 (capture q (* 2 (return 4 q)))))

;; Representation dependent wrt closures

(define closure-cps
  (lambda (x body env)
    (lambda (a k)
      (value-of-cps body (extend-env x a env) k))))

(define apply-closure-cps
  (lambda (c a k)
    (c a k)))

(define empty-k
  (lambda () identity))

(define value-of-cps
  (lambda (expr env k)
    (pmatch expr
      (`,n (guard (or (number? n) (boolean? n))) (k n))
      (`(+ ,n1 ,n2) (value-of-cps
                     n1 env
                     (lambda (n1)
                       (value-of-cps
                        n2 env
                        (lambda (n2)
                          (k (+ n1 n2)))))))
      (`(* ,n1 ,n2) (value-of-cps
                     n1 env
                     (lambda (n1)
                       (value-of-cps
                        n2 env
                        (lambda (n2)
                          (k (* n1 n2)))))))
      (`(sub1 ,n) (value-of-cps n env (lambda (n) (k (sub1 n)))))
      (`(zero? ,n) (value-of-cps n env (lambda (n) (k (zero? n)))))
      (`(if ,test ,conseq ,alt)
       (value-of-cps
        test env
        (lambda (b)
          (if b
              (value-of-cps conseq env k)
              (value-of-cps alt env k)))))
      (`(capture ,k-id ,body) (value-of-cps body (extend-env k-id k env) k))
      (`(return ,v-exp ,k-exp) (value-of-cps
                                k-exp env
                                (lambda (k)
                                  (value-of-cps
                                   v-exp env
                                   (lambda (v)
                                     (k v))))))
      (`,x (guard (symbol? x)) (k (apply-env env x)))
      (`(lambda (,x) ,body) (k (closure-cps x body env)))
      (`(,rator ,rand) (value-of-cps
                        rator env
                        (lambda (rator)
                          (value-of-cps
                           rand env
                           (lambda (rand)
                             (apply-closure-cps rator rand k)))))))))

;; Reprentation independent, functional continuations

(define empty-k-fn
  (lambda () identity))

(define apply-k-fn
  (lambda (k a)
    (k a)))

(define closure-cps-fn
  (lambda (x body env)
    (lambda (a k)
      (value-of-cps-fn body (extend-env x a env) k))))

(define inner-plus-k-fn
  (lambda (k n1)
    (lambda (n2)
      (apply-k-fn k (+ n1 n2)))))

(define outer-plus-k-fn
  (lambda (n2 env k)
    (lambda (n1)
      (value-of-cps-fn n2 env (inner-plus-k-fn k n1)))))

(define inner-times-k-fn
  (lambda (k n1)
    (lambda (n2)
      (apply-k-fn k (* n1 n2)))))

(define outer-times-k-fn
  (lambda (n2 k)
    (lambda (n1)
      (value-of-cps-fn
       n2 env (inner-times-k-fn k n1)))))

(define sub1-k-fn
  (lambda (k)
    (lambda (n)
      (apply-k-fn k (sub1 n)))))

(define zero?-k-fn
  (lambda (k)
    (lambda (n)
      (apply-k-fn k (zero? n)))))

(define if-k-fn
  (lambda (conseq alt env k)
    (lambda (b)
      (if b
          (value-of-cps-fn conseq env k)
          (value-of-cps-fn alt env k)))))

(define inner-return-k-fn
  (lambda (k)
    (lambda (v)
      (apply-k-fn k v))))

(define outer-return-k-fn
  (lambda (v-exp env)
    (lambda (k)
      (value-of-cps-fn
       v-exp env (inner-return-k-fn k)))))

(define inner-ap-k-fn
  (lambda (rator k)
    (lambda (rand)
      (apply-closure-cps rator rand k))))

(define outer-ap-k-fn
  (lambda (rand env k)
    (lambda (rator)
      (value-of-cps-fn rand env (inner-ap-k-fn rator k)))))

(define value-of-cps-fn
  (lambda (expr env k)
    (pmatch expr
      (`,n (guard (or (number? n) (boolean? n))) (apply-k-fn k n))
      (`(+ ,n1 ,n2) (value-of-cps-fn n1 env (outer-plus-k-fn n2 env k)))
      (`(* ,n1 ,n2) (value-of-cps-fn n1 env (outer-times-k-fn n2 k)))
      (`(sub1 ,n) (value-of-cps-fn n env (sub1-k-fn k)))
      (`(zero? ,n) (value-of-cps-fn n env (zero?-k-fn k)))
      (`(if ,test ,conseq ,alt) (value-of-cps-fn test env (if-k-fn conseq alt env k)))
      (`(capture ,k-id ,body) (value-of-cps-fn body (extend-env k-id k env) k))
      (`(return ,v-exp ,k-exp) (value-of-cps-fn k-exp env (outer-return-k-fn v-exp env)))
      (`,x (guard (symbol? x)) (apply-k-fn k (apply-env env x)))
      (`(lambda (,x) ,body) (apply-k-fn k (closure-cps-fn x body env)))
      (`(,rator ,rand) (value-of-cps-fn rator env (outer-ap-k-fn rand env k))))))

;; Representation independent, data structural continuations

(define closure-cps-ds
  (lambda (x body env)
    (lambda (a k)
      (value-of-cps-ds body (extend-env x a env) k))))

(define empty-k-ds
  (lambda ()
    '(empty-k)))

(define apply-k-ds
  (lambda (k v)
    (pmatch k
      (`(empty-k) v)
      (`(inner-plus-k-ds ,k ,n1) (apply-k-ds k (+ n1 v)))
      (`(outer-plus-k-ds ,n2 ,env ,k) (value-of-cps-ds n2 env (inner-plus-k-ds k v)))
      (`(inner-times-k-ds ,k ,n1) (apply-k-ds k (* n1 v)))
      (`(outer-times-k-ds ,n2 ,k) (value-of-cps-ds n2 env (inner-times-k-ds k v)))
      (`(sub1-k-ds ,k) (apply-k-ds k (sub1 v)))
      (`(zero?-k-ds ,k) (apply-k-ds k (zero? v)))
      (`(if-k-ds ,conseq ,alt ,env ,k) (if v
                                           (value-of-cps-ds conseq env k)
                                           (value-of-cps-ds alt env k)))
      (`(inner-return-k-ds ,k) (apply-k-ds k v))
      (`(outer-return-k-ds ,v-exp ,env) (value-of-cps-ds v-exp env (inner-return-k-ds v)))
      (`(inner-ap-k-ds ,rator ,k) (apply-closure-cps rator v k))
      (`(outer-ap-k-ds ,rand ,env ,k) (value-of-cps-ds rand env (inner-ap-k-ds v k))))))

(define inner-plus-k-ds
  (lambda (k n1)
    `(inner-plus-k-ds ,k ,n1)))

(define outer-plus-k-ds
  (lambda (n2 env k)
    `(outer-plus-k-ds ,n2 ,env ,k)))

(define inner-times-k-ds
  (lambda (k n1)
    `(inner-times-k-ds ,k ,n1)))

(define outer-times-k-ds
  (lambda (n2 k)
    `(outer-times-k-ds ,n2 ,k)))

(define sub1-k-ds
  (lambda (k)
    `(sub1-k-ds ,k)))

(define zero?-k-ds
  (lambda (k)
    `(zero?-k-ds ,k)))

(define if-k-ds
  (lambda (conseq alt env k)
    `(if-k-ds ,conseq ,alt ,env ,k)))

(define inner-return-k-ds
  (lambda (k)
    `(inner-return-k-ds ,k)))

(define outer-return-k-ds
  (lambda (v-exp env)
    `(outer-return-k-ds ,v-exp ,env)))

(define inner-ap-k-ds
  (lambda (rator k)
    `(inner-ap-k-ds ,rator ,k)))

(define outer-ap-k-ds
  (lambda (rand env k)
    `(outer-ap-k-ds ,rand ,env ,k)))

(define value-of-cps-ds
  (lambda (expr env k)
    (pmatch expr
      (`,n (guard (or (number? n) (boolean? n))) (apply-k-ds k n))
      (`(+ ,n1 ,n2) (value-of-cps-ds n1 env (outer-plus-k-ds n2 env k)))
      (`(* ,n1 ,n2) (value-of-cps-ds n1 env (outer-times-k-ds n2 k)))
      (`(sub1 ,n) (value-of-cps-ds n env (sub1-k-ds k)))
      (`(zero? ,n) (value-of-cps-ds n env (zero?-k-ds k)))
      (`(if ,test ,conseq ,alt) (value-of-cps-ds test env (if-k-ds conseq alt env k)))
      (`(capture ,k-id ,body) (value-of-cps-ds body (extend-env k-id k env) k))
      (`(return ,v-exp ,k-exp) (value-of-cps-ds k-exp env (outer-return-k-ds v-exp env)))
      (`,x (guard (symbol? x)) (apply-k-ds k (apply-env env x)))
      (`(lambda (,x) ,body) (apply-k-ds k (closure-cps-ds x body env)))
      (`(,rator ,rand) (value-of-cps-ds rator env (outer-ap-k-ds rand env k))))))

;; Brainteaser

(define-syntax cons$
  (syntax-rules ()
    ((cons$ x y) (cons x (delay y)))))

(define car$ car)

(define cdr$
  (lambda ($)
    (force (cdr $))))

(define zip-with-3
  (lambda (f l1 l2 l3)
    (cons$ (f (car$ l1) (car$ l2) (car$ l3))
           (zip-with-3 f (cdr$ l1) (cdr$ l2) (cdr$ l3)))))

(define trib$
  (cons$ 0 (cons$ 1 (cons$ 1 (zip-with-3 + trib$ (cdr$ trib$) (cdr$ (cdr$ trib$)))))))

;; Just Dessert

(define counter 0)

(define new-sym
  (lambda (var)
    (begin
      (set! counter (add1 counter))
      (string->symbol (format "~a~a" var counter)))))

(define new-k
  (lambda ()
    (new-sym 'k)))

(define new-var
  (lambda ()
    (new-sym 'x)))

(define convert-trivial
  (lambda (value)
    (pmatch value
      (`,x (guard (or (number? x) (symbol? x))) x)
      (`(lambda (,x) ,body) (let ((k (new-k)))
                              `(lambda (,x)
                                 (lambda (,k)
                                   ,(convert-expr body k))))))))

(define trivial?
  (lambda (exp)
    (pmatch exp
      (`,x (guard (or (number? x) (symbol? x))) #t)
      (`(lambda (,x) ,body) #t)
      (else #f))))

(define serious? (compose not trivial?))

(define convert-serious
  (lambda (comp)
    (pmatch comp
      (`(,rator ,rand)
       (cond
        ((and (trivial? rator) (trivial? rand)) `(,(convert-trivial rator) ,(convert-trivial rand)))
        ((and (trivial? rator) (serious? rand)) (let ((x (new-var)))
                                                  `(,(convert-serious rand)
                                                    (lambda (,x)
                                                      (,(convert-trivial rator) ,x)))))
        ((and (serious? rator) (trivial? rand)) (let ((x (new-var)))
                                                  `(,(convert-serious rator)
                                                    (lambda (,x) (,x ,(convert-trivial rand))))))
        ((and (serious? rator) (serious? rand)) (let ((x0 (new-var))
                                                      (x1 (new-var)))
                                                  `(,(convert-serious rator)
                                                    (lambda (,x0)
                                                      (,(convert-serious rand)
                                                       (lambda (,x1)
                                                         (,x0 ,x1))))))))))))

(define convert-expr
  (lambda (exp k)
    (if (trivial? exp)
        (list k (convert-trivial exp))
        (list (convert-serious exp) k))))

(define convert-cps
  (lambda (exp)
    (let ((k (new-k)))
      `(lambda (,k)
         ,(convert-expr exp k)))))
