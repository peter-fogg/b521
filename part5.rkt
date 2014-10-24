#lang racket

(require "parenthec.rkt")
(require C311/pmatch)

(define-union exp
  (const n)
  (var v)
  (if test conseq alt)
  (mult rand1 rand2)
  (sub1 rand)
  (zero rand)
  (capture body)
  (return vexp kexp)
  (let vexp body)
  (lambda body)
  (app rator rand))

(define-union cont
  (empty-k)
  (if-k conseq env alt k)
  (inner-mult-k n1 k)
  (outer-mult-k rand2 env k)
  (sub1-k k)
  (zero-k k)
  (inner-return-k kexp^)
  (outer-return-k vexp env)
  (let-k body env k)
  (inner-app-k rator^ k)
  (outer-app-k rand env k))

(define apply-k
  (lambda (k v)
    (union-case k cont
      ((empty-k) v)
      ((if-k conseq env alt k) (if v
                                   (let* ((expr conseq))
                                     (value-of expr env k))
                                   (let* ((expr alt))
                                     (value-of expr env k))))
      ((inner-mult-k n1 k) (let* ((v (* n1 v)))
                             (apply-k k v)))
      ((outer-mult-k rand2 env k) (let* ((k (cont_inner-mult-k v k))
                                         (expr rand2))
                                    (value-of expr env k)))
      ((sub1-k k) (let* ((v (- v 1)))
                    (apply-k k v)))
      ((zero-k k) (let* ((v (zero? v)))
                    (apply-k k v)))
      ((inner-return-k kexp^) (let* ((k kexp^))
                                (apply-k k v)))
      ((outer-return-k vexp env) (let* ((k (cont_inner-return-k v))
                                        (expr vexp))
                                   (value-of expr env k)))
      ((let-k body env k) (let* ((env (envr_extend v env))
                                 (expr body))
                            (value-of expr env k)))
      ((inner-app-k rator^ k) (let* ((c rator^)
                                     (a v))
                                (apply-closure c a k)))
      ((outer-app-k rand env k) (let* ((k (cont_inner-app-k v k))
                                       (expr rand))
                                  (value-of expr env k))))))

(define value-of
  (lambda (expr env k)
    (union-case expr exp
                [(const n) (let* ((v n))
                             (apply-k k v))]
                [(var v) (let* ((num v))
                           (apply-env env num k))]
                [(if test conseq alt)
                 (let* ((expr test)
                        (k (cont_if-k conseq env alt k)))
                   (value-of expr env k))]
                [(mult rand1 rand2) (let* ((k (cont_outer-mult-k rand2 env k))
                                           (expr rand1))
                                      (value-of expr env k))]
                [(sub1 rand) (let* ((k (cont_sub1-k k))
                                    (expr rand))
                               (value-of expr env k))]
                [(zero rand) (let* ((k (cont_zero-k k))
                                    (expr rand))
                               (value-of expr env k))]
                [(capture body)
                 (let* ((expr body)
                        (env (envr_extend k env)))
                   (value-of expr env k))]
                [(return vexp kexp)
                 (let* ((expr kexp)
                        (k (cont_outer-return-k vexp env)))
                   (value-of expr env k))]
                [(let vexp body)
                 (let* ((expr vexp)
                        (k (cont_let-k body env k)))
                   (value-of expr env k))]
                [(lambda body) (let* ((v (clos_closure body env)))
                                 (apply-k k v))]
                [(app rator rand)
                 (let* ((expr rator)
                        (k (cont_outer-app-k rand env k)))
                   (value-of expr env k))])))

(define-union envr
  (empty)
  (extend arg env))

(define apply-env
  (lambda (env num k)
    (union-case env envr
                [(empty) (let* ((v (error 'env "unbound variable")))
                           (apply-k k v))]
                [(extend arg env)
                 (if (zero? num)
                     (let* ((v arg))
                       (apply-k k v))
                     (let* ((num (sub1 num)))
                       (apply-env env num k)))])))

(define-union clos
  (closure code env))

(define apply-closure
  (lambda (c a k)
    (union-case c clos
                [(closure code env)
                 (let* ((expr code)
                        (env (envr_extend a env)))
                  (value-of code env k))])))

;;                                         ; Basic test...should be 5.
(pretty-print
 (value-of (exp_app
            (exp_app
             (exp_lambda (exp_lambda (exp_var 1)))
             (exp_const 5))
            (exp_const 6))
           (envr_empty)
           (cont_empty-k)))

                                        ; Factorial of 5...should be 120.
(pretty-print
 (value-of (exp_app
            (exp_lambda
             (exp_app
              (exp_app (exp_var 0) (exp_var 0))
              (exp_const 5)))
            (exp_lambda
             (exp_lambda
              (exp_if (exp_zero (exp_var 0))
                      (exp_const 1)
                      (exp_mult (exp_var 0)
                                (exp_app
                                 (exp_app (exp_var 1) (exp_var 1))
                                 (exp_sub1 (exp_var 0))))))))
           (envr_empty)
           (cont_empty-k)))

                                        ; Test of capture and return...should evaluate to 24.
(pretty-print
 (value-of
  (exp_mult (exp_const 2)
            (exp_capture
             (exp_mult (exp_const 5)
                       (exp_return (exp_mult (exp_const 2) (exp_const 6))
                                   (exp_var 0)))))
  (envr_empty)
  (cont_empty-k)))

;; (let ([fact (lambda (f)
;;               (lambda (n)
;;                 (if (zero? n)
;;                     1
;;                     (* n ((f f) (sub1 n))))))])
;;   ((fact fact) 5))

(pretty-print
 (value-of (exp_let
            (exp_lambda
             (exp_lambda
              (exp_if
               (exp_zero (exp_var 0))
               (exp_const 1)
               (exp_mult
                (exp_var 0)
                (exp_app
                 (exp_app (exp_var 1) (exp_var 1))
                 (exp_sub1 (exp_var 0)))))))
            (exp_app (exp_app (exp_var 0) (exp_var 0)) (exp_const 5)))
           (envr_empty)
           (cont_empty-k)))
