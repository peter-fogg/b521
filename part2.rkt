#lang racket
(require "parenthec.rkt")

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

(define empty-k
  (lambda ()
    identity))

(define if-k
  (lambda (conseq env alt k)
    (lambda (v)
      (if v
          (value-of conseq env k)
          (value-of alt env k)))))

(define inner-mult-k
  (lambda (n1 k)
    (lambda (n2)
      (apply-k k (* n1 n2)))))

(define outer-mult-k
  (lambda (rand2 env k)
    (lambda (n1)
      (value-of rand2 env (inner-mult-k n1 k)))))

(define sub1-k
  (lambda (k)
    (lambda (v)
      (apply-k k (- v 1)))))

(define zero-k
  (lambda (k)
    (lambda (v)
      (apply-k k (zero? v)))))

(define inner-return-k
  (lambda (kexp^)
    (lambda (vexp^)
      (apply-k kexp^ vexp^))))

(define outer-return-k
  (lambda (vexp env)
    (lambda (kexp^)
      (value-of vexp env (inner-return-k kexp^)))))

(define let-k
  (lambda (body env k)
    (lambda (v)
      (value-of body (envr_extend v env) k))))

(define inner-app-k
  (lambda (rator^ k)
    (lambda (rand^)
      (apply-closure rator^ rand^ k))))

(define outer-app-k
  (lambda (rand env k)
    (lambda (rator^)
      (value-of rand env (inner-app-k rator^ k)))))

(define value-of
  (lambda (expr env k)
    (union-case expr exp
                [(const n) (apply-k k n)]
                [(var v) (apply-env env v k)]
                [(if test conseq alt)
                 (value-of test env (if-k conseq env alt k))]
                [(mult rand1 rand2) (value-of rand1 env (outer-mult-k rand2 env k))]
                [(sub1 rand) (value-of rand env (sub1-k k))]
                [(zero rand) (value-of rand env (zero-k k))]
                [(capture body)
                 (value-of body (envr_extend k env) k)]
                [(return vexp kexp)
                 (value-of kexp env (outer-return-k vexp env))]
                [(let vexp body)
                 (value-of vexp env (let-k body env k))]
                [(lambda body) (apply-k k (clos_closure body env))]
                [(app rator rand)
                 (value-of rator env (outer-app-k rand env k))])))

(define-union envr
  (empty)
  (extend arg env))

(define apply-k
  (lambda (k a)
    (k a)))

(define apply-env
  (lambda (env num k)
    (union-case env envr
                [(empty) (k (error 'env "unbound variable"))]
                [(extend arg env)
                 (if (zero? num)
                     (apply-k k arg)
                     (apply-env env (sub1 num) k))])))

(define-union clos
  (closure code env))

(define apply-closure
  (lambda (c a k)
    (union-case c clos
                [(closure code env)
                 (value-of code (envr_extend a env) k)])))

;;                                         ; Basic test...should be 5.
(pretty-print
 (value-of (exp_app
            (exp_app
             (exp_lambda (exp_lambda (exp_var 1)))
             (exp_const 5))
            (exp_const 6))
           (envr_empty)
           (empty-k)))

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
           (empty-k)))

                                        ; Test of capture and return...should evaluate to 24.
(pretty-print
 (value-of
  (exp_mult (exp_const 2)
            (exp_capture
             (exp_mult (exp_const 5)
                       (exp_return (exp_mult (exp_const 2) (exp_const 6))
                                   (exp_var 0)))))
  (envr_empty)
  (empty-k)))

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
           (empty-k)))
