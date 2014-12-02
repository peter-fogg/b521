#lang racket

;; Peter Fogg

(require C311/pmatch)
(require C311/monads)

;; 1

(define assv-maybe
  (lambda (x l)
    (cond
     ((null? l) (fail))
     ((eqv? x (car (car l))) `(Just ,(cdr (car l))))
     (else (assv-maybe x (cdr l))))))

;; 2

(define partition-writer
  (lambda (f l)
    (cond
     ((null? l) (return-writer '()))
     ((f (car l)) (bind-writer
                   (tell-writer (car l))
                   (lambda (_)
                     (partition-writer f (cdr l)))))
     (else (bind-writer
            (partition-writer f (cdr l))
            (lambda (l^)
              (return-writer (cons (car l) l^))))))))

;; 3

(define powerXpartials
  (lambda (x n)
    (cond
     ((zero? n) (return-writer 1))
     ((= n 1) (return-writer x))
     ((odd? n) (do bind-writer
                 (res <- (powerXpartials x (sub1 n)))
                 (tell-writer res)
                 (return-writer (* x res))))
     (else (do bind-writer
             (res <- (powerXpartials x (/ n 2)))
             (tell-writer res)
             (return-writer (* res res)))))))

;; 4

(define abc-game
  (lambda (l)
    (if (null? l)
        (return-state '__)
        (do bind-state
          (score <- get-state)
          (put-state (+ score
                        (cond
                         ((eqv? 'a (car l)) 1)
                         ((eqv? 'b (car l)) -1)
                         (else 0))))
          (abc-game (cdr l))))))

;; 5

(define traverse
  (lambda (return bind f)
    (letrec ((trav (lambda (tree)
                     (cond
                      ((pair? tree)
                       (do bind
                         (a <- (trav (car tree)))
                         (b <- (trav (cdr tree)))
                         (return (cons a b))))
                      (else (f tree))))))
      trav)))

(define reciprocal
  (lambda (n)
    (if (zero? n)
        '(Nothing)
        (return-maybe (/ 1 n)))))

(define traverse-reciprocal
  (traverse return-maybe bind-maybe reciprocal))

;; 6

(define halve
  (lambda (n)
    (if (even? n)
        (return-writer (/ n 2))
        (bind-writer (tell-writer n)
                     (lambda (_) (return-writer n))))))

(define traverse-halve
  (traverse return-writer bind-writer halve))

;; 7

(define state/sum
  (lambda (n)
    (do bind-state
      (x <- get-state)
      (put-state (+ x n))
      (return-state x))))

(define traverse-state/sum
  (traverse return-state bind-state state/sum))

;; 8

(define apply-env
  (lambda (env x)
    (env x)))

(define extend-env
  (lambda (x a env)
    (lambda (y)
      (if (eqv? x y)
          a
          (apply-env env y)))))

(define empty-env
  (lambda ()
    (lambda (x)
      (error "unbound variable" x))))

(define closure
  (lambda (x body env)
    (lambda (a)
      (value-of-cps body (extend-env x a env)))))

(define apply-proc
  (lambda (c a)
    (c a)))

(define value-of-cps
  (lambda (expr env)
    (pmatch expr
      [`,n (guard (or (number? n) (boolean? n))) (return-cont n)]
      [`,x (guard (symbol? x)) (return-cont (apply-env env x))]
      [`(* ,x1 ,x2) (do bind-cont
                      (c1 <- (value-of-cps x1 env))
                      (c2 <- (value-of-cps x2 env))
                      (return-cont (* c1 c2)))]
      [`(sub1 ,x) (bind-cont (value-of-cps x env)
                             (lambda (n)
                               (return-cont (sub1 n))))]
      [`(zero? ,x) (bind-cont (value-of-cps x env)
                              (lambda (n)
                                (return-cont (zero? n))))]
      [`(if ,test ,conseq ,alt) (bind-cont (value-of-cps test env)
                                      (lambda (b)
                                        (if b
                                            (value-of-cps conseq env)
                                            (value-of-cps alt env))))]
      [`(capture ,k-id ,body) (callcc (lambda (k)
                                        (value-of-cps body (extend-env k-id k env))))]
      [`(return ,v-exp ,k-exp) (do bind-cont
                                 (k <- (value-of-cps k-exp env))
                                 (v <- (value-of-cps v-exp env))
                                 (k v))]
      [`(lambda (,id) ,body) (return-cont (closure id body env))]
      [`(,rator ,rand) (do bind-cont
                         (t <- (value-of-cps rator env))
                         (n <- (value-of-cps rand env))
                         (apply-proc t n))])))

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
