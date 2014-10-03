#lang racket

;; Peter Fogg

(require c311/pmatch)

(define make-closure
  (lambda (f)
    (lambda (x body env)
      (lambda (a)
        (f body (extend-env x a env))))))

(define empty-env
  (lambda ()
    (lambda (y)
      (error "unbound variable ~s!" y))))

(define apply-env
  (lambda (env x)
    (env x)))

(define extend-env
  (lambda (x a env)
    (lambda (y)
      (if (eqv? y x)
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
  (lambda (exp env)
    (pmatch exp
      (`,b (guard (boolean? b)) b)
      (`,n (guard (number? n)) n)
      (`(zero? ,n) (zero? (value-of n env)))
      (`(sub1 ,n) (sub1 (value-of n env)))
      (`(* ,n1 ,n2) (* (value-of n1 env) (value-of n2 env)))
      (`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env)))
      (`(begin2 ,e1 ,e2) (begin (value-of e1 env) (value-of e2 env)))
      (`(random ,n) (random (value-of n env)))
      (`,x (guard (symbol? x)) (apply-env env x))
      (`(lambda (,x) ,body) (closure x body env))
      (`(,rator ,rand) (apply-closure (value-of rator env) (value-of rand env))))))

;; Call by value

(define val-of-cbv
  (lambda (exp env)
    (pmatch exp
      (`,b (guard (boolean? b)) b)
      (`,n (guard (number? n)) n)
      (`,x (guard (symbol? x)) (unbox (apply-env env x)))
      (`(zero? ,n) (zero? (val-of-cbv n env)))
      (`(sub1 ,n) (sub1 (val-of-cbv n env)))
      (`(add1 ,n) (add1 (val-of-cbv n env)))
      (`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env)))
      (`(quote ()) '())
      (`(car ,l) (car (val-of-cbv l env)))
      (`(cdr ,l) (cdr (val-of-cbv l env)))
      (`(cons ,x ,xs) (cons (val-of-cbv x env) (val-of-cbv xs env)))
      (`(car^ ,l) (let* ((b (car (val-of-cbv l env))) ;; Not sure if the caching
                         (v (unbox b)))               ;; is necessary here, but
                    (if (procedure? v)                ;; I'm doing it anyway
                        (let ((val (v)))
                          (set-box! b val)
                          val)
                        v)))
      (`(cdr^ ,l) (let* ((b (cdr (val-of-cbv l env)))
                         (v (unbox b)))
                    (if (procedure? v)
                        (let ((val (v)))
                          (set-box! b val)
                          val)
                        v)))
      (`(cons^ ,x ,xs) (cons (box (lambda () (val-of-cbv x env))) (box (lambda () (val-of-cbv xs env)))))
      (`(null? ,l) (null? (val-of-cbv l env)))
      (`(let ,pairs ,body) (val-of-cbv body (foldl (lambda (x acc)
                                                     (extend-env (car x)
                                                                 (box (val-of-cbv (cadr x) env))
                                                                 acc))
                                                   env
                                                   pairs)))
      (`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                    (val-of-cbv conseq env)
                                    (val-of-cbv alt env)))
      (`(set! ,var ,value) (set-box! (apply-env env var) (val-of-cbv value env)))
      (`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env)))
      (`(random ,n) (random (val-of-cbv n env)))
      (`(lambda (,x) ,body) ((make-closure val-of-cbv) x body env))
      (`(,rator ,rand) (apply-closure (val-of-cbv rator env) (box (val-of-cbv rand env)))))))

;; Call by reference

(define closure-cbr
  (lambda (x body env)
    (lambda (a)
      (val-of-cbr body (extend-env x a env)))))

(define val-of-cbr
  (lambda (exp env)
    (pmatch exp
      (`,b (guard (boolean? b)) b)
      (`,n (guard (number? n)) n)
      (`,x (guard (symbol? x)) (unbox (apply-env env x)))
      (`(zero? ,n) (zero? (val-of-cbr n env)))
      (`(sub1 ,n) (sub1 (val-of-cbr n env)))
      (`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env)))
      (`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                    (val-of-cbr conseq env)
                                    (val-of-cbr alt env)))
      (`(set! ,var ,value) (set-box! (apply-env env var) (val-of-cbr value env)))
      (`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env)))
      (`(random ,n) (random (val-of-cbr n env)))
      (`(lambda (,x) ,body) ((make-closure val-of-cbr) x body env))
      (`(,rator ,x) (guard (symbol? x)) (apply-closure (val-of-cbr rator env) (apply-env env x)))
      (`(,rator ,rand) (apply-closure (val-of-cbr rator env) (box (val-of-cbr rand env)))))))

;; Call by name

(define val-of-cbname
  (lambda (exp env)
    (pmatch exp
      (`,b (guard (boolean? b)) b)
      (`,n (guard (number? n)) n)
      (`,x (guard (symbol? x)) ((apply-env env x)))
      (`(zero? ,n) (zero? (val-of-cbname n env)))
      (`(sub1 ,n) (sub1 (val-of-cbname n env)))
      (`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env)))
      (`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                    (val-of-cbname conseq env)
                                    (val-of-cbname alt env)))
      (`(random ,n) (random (val-of-cbname n env)))
      (`(lambda (,x) ,body) ((make-closure val-of-cbname) x body env))
      (`(,rator ,rand) (apply-closure (val-of-cbname rator env) (lambda () (val-of-cbname rand env)))))))

;; Call by need

;; A few helpers...

(define-syntax thunk
  (syntax-rules ()
    ((_ value) (box (list 'thunk (lambda () value))))))

(define eval-thunk
  (lambda (b)
    (let ((v (unbox b)))
      (pmatch v
        (`(thunk ,t) (let ((val (t)))
                       (set-box! b `(forced ,val))
                       val))
        (`(forced ,val) val)))))

(define val-of-cbneed
  (lambda (exp env)
    (pmatch exp
      (`,b (guard (boolean? b)) b)
      (`,n (guard (number? n)) n)
      (`,x (guard (symbol? x)) (eval-thunk (apply-env env x)))
      (`(zero? ,n) (zero? (val-of-cbneed n env)))
      (`(sub1 ,n) (sub1 (val-of-cbneed n env)))
      (`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env)))
      ;; Adding some stuff to make loeb work later on...
      (`(car ,l) (eval-thunk (car (val-of-cbneed l env))))
      (`(cdr ,l) (eval-thunk (cdr (val-of-cbneed l env))))
      (`(cons ,x ,xs) (cons (thunk (val-of-cbneed x env))
                            (thunk (val-of-cbneed xs env))))
      (`(null? ,l) (null? (val-of-cbneed l env)))
      (`(quote ()) '())
      (`(set! ,var ,value) (set-box! (apply-env env var) (thunk (val-of-cbneed value env))))
      (`(begin2 ,e1 ,e2) (begin (val-of-cbneed e1 env) (val-of-cbneed e2 env)))
      ;; End loeb extras
      (`(let ,pairs ,body) (val-of-cbneed body (foldl (lambda (x acc)
                                                        (extend-env (car x)
                                                                    (thunk (val-of-cbneed (cadr x) env))
                                                                    acc))
                                                      env
                                                      pairs)))
      (`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                    (val-of-cbneed conseq env)
                                    (val-of-cbneed alt env)))
      (`(random ,n) (random (val-of-cbneed n env)))
      (`(lambda (,x) ,body) ((make-closure val-of-cbneed) x body env))
      (`(,rator ,rand) (apply-closure (val-of-cbneed rator env) (thunk (val-of-cbneed rand env)))))))

;; Loeb. This doesn't quite work, unfortunately -- I haven't been able
;; to figure out why... I made the list functions in val-of-cbneed
;; lazy because (I think) that's necessary in order to get sensible
;; behavior out of the spreadsheet-like use case for loeb, but I
;; haven't worked out a way to evaluate the thunks at the right
;; time. Part of the issue is that there's a mixing of the strict
;; Racket interpreter with the lazy val-of-cbneed interpreter, which
;; means that evaluating something like (cons (+ 1 2) '()) results in
;; a list of thunks. There's no simple way that I can see to ensure
;; that the "user" of the val-of-cbneed interpreter only ever sees
;; fully evaluated data. Oh well. Loeb is neat anyway.

;; As an aside, does Racket have any facility for abstracting over the
;; map in loeb to get moeb for any functor?

(define loeb-test
  '(let ((fix (lambda (f)
                ((lambda (x) (f (lambda (v) ((x x) v))))
                 (lambda (x) (f (lambda (v) ((x x) v))))))))
     (let ((map (fix (lambda (map)
                       (lambda (f)
                         (lambda (l)
                           (if (null? l)
                               '()
                               (cons (f (car l)) ((map f) (cdr l))))))))))
       (let ((loeb (lambda (x)
                     (let ((go-standin #f))
                       (let ((go ((map (lambda (f) (f go-standin))) x)))
                         (begin2
                          (set! go-standin go)
                          go))))))
         (let ((l (cons (lambda (l) 1) (cons (lambda (l) (car l)) '()))))
           (loeb l))))))
