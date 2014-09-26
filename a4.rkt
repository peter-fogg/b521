 #lang racket
;; Peter Fogg

(require c311/pmatch)

;; 1

(define empty-env-fn
  (lambda ()
    (lambda (y) (error (format "unbound variable reference ~s!" y)))))

(define empty-env empty-env-fn)

(define extend-env-fn
  (lambda (x a env)
    (lambda (y)
      (if (eqv? y x)
          a
          (env y)))))

(define apply-env-fn
  (lambda (env x)
    (env x)))

(define closure-fn
  (lambda (x body env)
    (lambda (a)
      (value-of-fn body (extend-env-fn x a env)))))

(define apply-closure-fn
  (lambda (c a)
    (c a)))

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
      (`(lambda (,x) ,body) (closure-fn x body env))
      (`(,rator ,rand) (apply-closure-fn (value-of-fn rator env) (value-of-fn rand env))))))

;; 2

(define empty-env-ds
  (lambda ()
    '(empty-env-ds)))

(define extend-env-ds
  (lambda (x a env)
    `(extend-env-ds ,x ,a ,env)))

(define apply-env-ds
  (lambda (env x)
    (pmatch env
      (`(empty-env-ds) (error (format "unbound variable reference ~s!" x)))
      (`(extend-env-ds ,y ,a ,rest) (if (eqv? y x)
                                        a
                                        (apply-env-ds rest x))))))

(define closure-ds
  (lambda (x body env)
    `(closure ,x ,body ,env)))

(define apply-closure-ds
  (lambda (c a)
    (pmatch c
      (`(closure ,x ,body ,env) (value-of-ds body (extend-env-ds x a env))))))

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
      (`(lambda (,x) ,body) (closure-ds x body env))
      (`(,rator ,rand) (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))))))

;; 3

(define closure-scopes
  (lambda (x body env lex)
    (if lex
        `(closure ,x ,body ,env)
        `(closure ,x ,body))))

(define apply-closure-scopes
  (lambda (c a env^)
    (pmatch c
      (`(closure ,x ,body ,env) (value-of-scopes body (extend-env-ds x a env)))
      (`(closure ,x ,body) (value-of-scopes body (extend-env-ds x a env^))))))

(define extend-env extend-env-ds)

(define value-of-scopes
  (lambda (exp env)
    (pmatch exp
      (`,n (guard (number? n)) n)
      (`,b (guard (boolean? b)) b)
      (`(quote ()) '())
      (`,x (guard (symbol? x)) (apply-env-ds env x))
      (`(null? ,l) (null? (value-of-scopes l env)))
      (`(cons ,a ,d) (cons (value-of-scopes a env) (value-of-scopes d env)))
      (`(car ,l) (car (value-of-scopes l env)))
      (`(cdr ,l) (cdr (value-of-scopes l env)))
      (`(zero? ,e) (if (zero? (value-of-scopes e env))
                       #t
                       #f))
      (`(sub1 ,n) (sub1 (value-of-scopes n env)))
      (`(* ,n1 ,n2) (* (value-of-scopes n1 env) (value-of-scopes n2 env)))
      (`(if ,test-exp ,then-exp ,else-exp) (if (value-of-scopes test-exp env)
                                               (value-of-scopes then-exp env)
                                               (value-of-scopes else-exp env)))
      (`(let ((,var ,value)) ,body) (value-of-scopes body (extend-env-ds var (value-of-scopes value env) env)))
      (`(lambda (,x) ,body) (closure-scopes x body env #t))
      (`(d-lambda (,x) ,body) (closure-scopes x body env #f))
      (`(,rator ,rand) (apply-closure-scopes (value-of-scopes rator env) (value-of-scopes rand env) env)))))

;; 4

(define closure-fn-ri
  (lambda (x body env extras)
    (lambda (a)
      ((car extras) body ((cdr extras) x a env)))))

(define apply-closure-fn-ri
  (lambda (c a _)
    (c a)))

(define closure-ds-ri
  (lambda (x body env _)
    `(closure ,x ,body ,env)))

(define apply-closure-ds-ri
  (lambda (c a extras)
    (pmatch c
      (`(closure ,x ,body ,env) ((car extras) body ((cdr extras) x a env))))))

(define value-of-ri
  (lambda (empty-env extend-env apply-env closure apply-closure)
    (lambda (exp)
      (letrec ((go (lambda (exp env)
                     (pmatch exp
                       (`,n (guard (number? n)) n)
                       (`,b (guard (boolean? b)) b)
                       (`,x (guard (symbol? x)) (apply-env env x))
                       (`(zero? ,n) (zero? (go n env)))
                       (`(sub1 ,n) (sub1 (go n env)))
                       (`(* ,n1 ,n2) (* (go n1 env) (go n2 env)))
                       (`(if ,test-exp ,then-exp ,else-exp) (if (go test-exp env)
                                                                (go then-exp env)
                                                                (go else-exp env)))
                       (`(let ((,var ,value)) ,body) (go body (extend-env env var value)))
                       (`(lambda (,x) ,body) (closure x body env (cons go extend-env)))
                       (`(,rator ,rand) (apply-closure (go rator env) (go rand env) (cons go extend-env)))))))
        (go exp (empty-env))))))
