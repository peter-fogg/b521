;; 8-26

;; B521

;; Dan Friedman

;; ================================================================================

;; Scheme:
;;   - car
;;   - cdr
;;   - cons
;;   - null?
;;   - length

;;   - arithmetic: (- n1 n2) is monus, (sub1 n) = n - 1 if n > 0 else 0

;; ================================================================================

;; Sets:
;;   - disallow repeated elements
;;   - support membership testing
;;   (define member?
;;     (lambda (a set)
;;       (cond
;;        ((null? set) #f)
;;        ((eqv? a (car set)) #t)
;;        (else (member? a (cdr set))))))
;;   (define rember
;;     (lambda (a l)
;;       (cond
;;        ((null? l) '())
;;        ((eqv? a (car l)) (cdr l))
;;        (else (cons (car l) (rember a (cdr l)))))))
;;   (define plus
;;     (lambda (n1 n2)
;;       (cond
;;        ((zero? n2) n1)
;;        (else (add1 (plus n1 (sub1 n2)))))))
;;   (define mult
;;     (lambda (n1 n2)
;;       (cond
;;        ((zero? n2) 0)
;;        (else (plus n1 (mult n1 (sub1 n2)))))))

;; ================================================================================

;; 8-28

;; (define rember
;;     (lambda (a l)
;;       (cond
;;        ((null? l) '())
;;        ((eqv? (car l) a) (rember a (cdr l)))
;;        (else (cons (car l) (rember a (cdr l)))))))

;; (define rember*
;;     (lambda (a l)
;;       (cond
;;        ((null? l) '())
;;        ((pair? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
;;        ((eqv? (car l) a) (rember* a (cdr l)))
;;        (else (cons (car l) (rember* a (cdr l)))))))

;; (define all-bool?
;;     (lambda (l)
;;       (cond
;;        ((null? l) #t)
;;        ((pair? (car l)) (and (all-bool? (car l)) (all-bool? (cdr l))))
;;        ((boolean? (car l)) (all-bool? (cdr l)))
;;        (else #f))))

;; - Complicated data structures don't require complicated functions

;; ================================================================================

;; (define plus
;;   (lambda (n1 n2)
;;     (cond
;;      ((zero? n2) n1)
;;      (else (add1 (plus n1 (sub1 n2)))))))

;; (define mult
;;     (lambda (n1 n2)
;;       (cond
;;        ((zero? n2) 0)
;;        (else (plus n1 (mult n1 (sub1 n2)))))))

;; (define arrow
;;     (lambda (n1 n2)
;;       (cond
;;        ((zero? n2) 1)
;;        (else (mult n1 (arrow n1 (sub1 n2)))))))

;; (define double-arrow
;;     (lambda (n1 n2)
;;       (cond
;;        ((zero? n2) 1)
;;        (else (arrow n1 (double-arrow n1 (sub1 n2)))))))

;; (define triple-arrow
;;     (lambda (n1 n2)
;;       (cond
;;        ((zero? n2) 1)
;;        (else (double-arrow n1 (triple-arrow n1 (sub1 n2)))))))

;; (define G
;;     (lambda (i)
;;       (lambda (n1 n2)
;;         (cond
;;          ((zero? i) (cond
;;                      ((zero? n2) n1)
;;                      (else (add1 ((G 0) n1 (sub1 n2))))))
;;          ((eqv? i 1) (cond
;;                       ((zero? n2) 0)
;;                       (else ((G (sub1 i)) n1 ((G i) n1 (sub1 n2))))))
;;          (else (cond
;;                 ((zero? n2) 1)
;;                 (else ((G (sub1 i)) n1 ((G i) n1 (sub1 n2))))))))))

;; (define GG
;;     (lambda (i)
;;       (lambda (n1 n2)
;;         (cond
;;          ((zero? i) (cond
;;                      ((zero? n2) n1)
;;                      (else (add1 ((GG 0) n1 (sub1 n2))))))
;;          ((eqv? i 1) (cond
;;                       ((zero? n2) 0)
;;                       (else ((GG (sub1 i)) n1 ((GG i) n1 (sub1 n2))))))
;;          (else (cond
;;                 ((zero? n2) 1)
;;                 (else ((GG (sub1 i)) n1 ((GG i) n1 (sub1 n2))))))))))

;; - ... correctness-preserving transformations ...

;; (define GG
;;     (lambda (i n1 n2)
;;       (cond
;;        ((and (zero? n2) (zero? i)) n1)
;;        ((and (zero? n2) (zero? (sub1 i))) 0)
;;        ((zero? n2) 1)
;;        ((zero? i) (add1 (GG i n1 (sub1 n2))))
;;        (else (GG (sub1 i) n1 (GG i n1 (sub1 n2)))))))

;; (define ackerman GG) ;; yep!

;; ================================================================================

;; 9-2

;; - Our language is the lambda calculus.
;;   - (lambda (x) body) : lambda term
;;   - (M_1 M_2) : application
;;   - x : variable

;; - pmatch! pattern matching
;;   - ` : quasiquote
;;   - ' : quote
;;   - , : unquote
;;   - > `(mary had a little lamb)
;;       '(mary had a little lamb)
;;   - > (let ((name 'mary))
;;         `(,name had a little lamb))
;;       '(mary had a little lamb)
;;   - unquote evaluates expressions

;; (define -length
;;   (lambda (l)
;;     (pmatch l
;;       (`(,a . ,d) (+ 1 (-length d)))
;;       (else 0))))

;; (define cons-cell-count
;;   (lambda (tr)
;;     (pmatch tr
;;       (`(,a . ,d) (+ 1 (cons-cell-count a) (cons-cell-count d)))
;;       (else 0))))

;; (define stuff
;;   (lambda (e)
;;     (pmatch e
;;       (`,x (guard (symbol? x)) 'foo)
;;       (`(lambda (,x) ,body) 'foo)
;;       (`(,rator ,rand) 'foo))))

;; ================================================================================

;; 9-4

;; (define assv
;;   (lambda (x al)
;;     (pmatch al
;;       (`() #f)
;;       (`(,A . ,D)
;;        (cond
;;         ((eqv? (car A) x) A)
;;         (else (assv x D)))))))

;; - De Bruijn indices

;; (define map-alist ;; maybe useful?
;;   (lambda (f al)
;;     (pmatch al
;;       (`() '())
;;       (`(,A . ,D) (cons (cons (car A) (f (cdr A))) (map-alist f D))))))

;; (define var-occurs-free?
;;   (lambda (x e cenv)
;;     (pmatch-who "vof?" e
;;       (`,y (guard (symbol? y)) (and (eqv? y x) (not (memv x cenv))))
;;       (`(lambda (,y) ,body) (var-occurs-free? x body (cons y cenv)))
;;       (`(,rator ,rand) (or (var-occurs-free? x rator cenv)
;;                            (var-occurs-free? x rand cenv))))))

;; (define var-occurs-bound?
;;   (lambda (x e cenv)
;;     (pmatch-who "vob?" e
;;       (`,y (guard (symbol? y)) (and (eqv? y x) (memv x cenv)))
;;       (`(lambda (,y) ,body) (var-occurs-bound? x body (cons y cenv)))
;;       (`(,rator ,rand) (or (var-occurs-bound? x rator cenv)
;;                            (var-occurs-bound? x rand cenv))))))

;; ================================================================================

;; 9-9

;; - The lambda calculus!

;; (define valof
;;   (lambda (exp env)
;;     (pmatch exp
;;       (`,n (guard (number? n)) n)
;;       (`,x (guard (symbol? x)) (env x))
;;       (`(lambda (,x) ,body) (lambda (a) (valof body (lambda (y) (if (eqv? y x) a (env y))))))
;;       (`(zero? ,n-exp) (zero? (valof n-exp env)))
;;       (`(,rator ,rand) ((valof rator env) (valof rand env))))))

;; ================================================================================

;; - Representation independence in interpreters

;; (define valof
;;   (lambda (exp env)
;;     (pmatch exp
;;       (`,x (guard (symbol? x)) (apply-env env x))
;;       (`(lambda (,x) ,body) (lambda (a) (valof body (extend-env x a env))))
;;       (`(,rator ,rand) ((valof rator env) (valof rand env))))))

;; (define extend-env
;;   (lambda (x a env)
;;      (lambda (y) (if (eqv? x y) a (apply-env env y)))))

;; (define empty-env
;;   (lambda ()
;;     (lambda (y)
;;       (error "empty environment"))))

;; (define apply-env
;;   (lambda (env x)
;;     (env x)))

;; - Or we can write:

;; (define apply-env
;;   (lambda (env x)
;;     (pmatch env
;;       (`(empty-env) (error "empty environment"))
;;       (`(extend-env ,x ,a ,env) (if (eqv? x y)
;;                                     a
;;                                     (apply-env env x))))))

;; (define extend-env
;;   (lambda (x a env)
;;     `(extend-env ,x ,a ,env)))

;; (define empty-env
;;   (lambda ()
;;     '(empty-env)))

;; - So we have two different representations for the same problem --
;;   valof doesn't care which environment type we have.

;; - With abstraction over closures:

;; (define valof
;;   (lambda (exp env)
;;     (pmatch exp
;;       (`,x (guard (symbol? x)) (apply-env env x))
;;       (`(lambda (,x) ,body) (closure x body env))
;;       (`(,rator ,rand) (apply-closure (valof rator env) (valof rand env))))))

;; (define closure
;;   (lambda (x body env)
;;     (lambda (a)
;;       (valof body (extend-env x a env)))))

;; (define apply-closure
;;   (lambda (p a)
;;     (p a)))

;; -- Or:

;; (define closure
;;   (lambda (x body env)
;;     `(closure ,x ,body ,env)))

;; (define apply-closure
;;   (lambda (p a)
;;     (pmatch p
;;       (`(closure ,x ,body ,env) (valof body (extend-env x a env))))))

;; - Recursion with omega combinator:
;;   ((lambda (f) (f f))
;;    (lambda (f) (f f)))

;; - Recursion with Y!
;; (define Y
;;   (lambda (f)
;;     ((lambda (x) (x x))
;;      (lambda (x) (f (lambda (y) ((x x) y)))))))

;; (define fac
;;   (lambda (recur)
;;     (lambda (n)
;;       (if (zero? n)
;;           1
;;           (* n (recur (sub1 n)))))))

;; ================================================================================

;; 9-16

;; - Dynamic and lexical scope

;; > (let ((a 1))
;; >   (let ((whats-a? (lambda (_) a)))
;; >     (let ((a 2))
;; >       (whats-a? 0))))

;; - In Racket, returns 1
;; - In elisp (with `(funcall whats-a? 0)`), returns 2

;; - `dlambda` gives dynamic scope, `lambda` lexical scope:
;; > (define valof
;; >   (lambda (e env)
;; >     (pmatch e
;; >       (`,x (guard (symbol? x)) (env x))
;; >       (`(lambda (,x) ,body) (lambda (a env) (valof body (lambda (y)
;; >                                                           (if (eqv? y x)
;; >                                                               a
;; >                                                               (env y))))))
;; >       (`(dlambda (,x) ,body) (lambda (a env^) (valof body (lambda (y)
;; >                                                             (if (eqv? y x)
;; >                                                                 a
;; >                                                                 (env^ y))))))
;; >       (`(,rator ,rand) ((valof rator env) (valof rand env) env)))))

;; ================================================================================

;; 9-18

;; - Boxes

;; > (let ((x 5))
;;     (box x))
;; > (set-box! a-box 120)

;; - Side effects in the language -- set!

;; > (define valof-mut
;; >   (lambda (exp env)
;; >     (pmatch exp
;; >       (`,x (guard (symbol? x)) (unbox (env x)))
;; >       (`(lambda (,x) ,body) (lambda (a) (valof-mut body (lambda (y)
;; >                                                       (if (eqv? y x)
;; >                                                           (box a)
;; >                                                           (env y))))))
;; >       (`(,rator ,x) (guard (symbol? x)) ((valof-mut rator env) (box (unbox (env x)))))
;; >       (`(,rator ,rand) ((valof-mut rator env) (box (valof-mut rand env)))))))

;; - Laziness (call-by-name, call-by-need)

;; > (define apply-env
;; >   (lambda (env x)
;; >     (env x)))
;; > 
;; > (define extend-env
;; >   (lambda (x a env)
;; >     (lambda (y)
;; >       (if (eqv? y x)
;; >           (box (lambda () a))
;; >           (apply-env y)))))
;; > 
;; > (define valof-name
;; >   (lambda (exp env)
;; >     (pmatch exp
;; >       (`,x (guard (symbol? x)) ((unbox (apply-env env x))))
;; >       (`(lambda (,x) ,body) (lambda (a) (valof-name body (extend-env x a env))))
;; >       (`(,rator ,x) (guard (symbol? x)) ((valof-name rator env) (box (unbox (apply-env env x)))))
;; >       (`(,rator ,rand) ((valof-name rator env) (box (lambda () (valof-name rand env))))))))
;; > 
;; > (define valof-need
;; >   (lambda (exp env)
;; >     (pmatch exp
;; >       (`,x (guard (symbol? x)) (let* ((b (apply-env env x))
;; >                                       (val ((unbox b))))
;; >                                  (begin (set! b (lambda () val)) val)))
;; >       (`(lambda (,x) ,body) (lambda (a) (valof-need body (extend-env x a env))))
;; >       (`(,rator ,x) (guard (symbol? x)) ((valof-need rator env) (box (unbox (apply-env env x)))))
;; >       (`(,rator ,rand) ((valof-need rator env) (box (lambda () (valof-need rand env))))))))

;; ================================================================================

;; 9-30

;; - Controlling order of evaluation
;;   - If (h (f x) (g y)), which of f and g are evaluated first?
;;   - Make explicit with
;;   > (let ((a (f x)))
;;   >   (let ((b (g y)))
;;   >     (h a b)))
;;   - A-normal form.
;;   > (if (h (f x) (g y))
;;   >     (r s)
;;   >     (t u))
;;   - How do we ANF this?
;;   > (let ((a (f x)))
;;   >   (let ((b (g y)))
;;   >     (let ((bool (h a b)))
;;   >       (if bool
;;   >           (r s)
;;   >           (t u)))))
;;   - All arguments to functions are just variable lookups.
;;   - CPS
;;   - Transform original programs to:
;;   > (f x (lambda (a)
;;   >        (g y (lambda (b)
;;   >               (h a b (lambda (x) x))))
;;   > (f x (lambda (a)
;;   >        (g y (lambda (b)
;;   >               (h a b (lambda (bool)
;;   >                        (if bool
;;   >                            (r s k)
;;   >                            (t u k))))))))
;;   - Factorial
;;   > (define !
;;   >   (lambda (n k)
;;   >     (if (zero? n)
;;   >         (k 1)
;;   >         (! (sub1 n) (lambda (m)
;;   >                       (k (* m n)))))))
;;   > (define filter-cps
;;   >   (lambda (p ls k)
;;   >     (cond
;;   >      ((null? ls) (k '()))
;;   >      (else (p (car ls) (lambda (b)
;;   >                          (if b
;;   >                              (filter-cps p (cdr ls) (lambda (ls^) (k (cons (car ls) ls^))))
;;   >                              (filter-cps p (cdr ls) k))))))))
;;   - Eta expansion: if we have (lambda (a) (f a))
;;     - f cannot loop,
;;     - a is a variable,
;;     - a is not free in f,
;;     - then we can eta-reduce to f.
;;   - Alternatively,
;;   > (define filter-cps^
;;   >   (lambda (p ls k)
;;   >     (cond
;;   >      ((null? ls) (k '()))
;;   >      (else (p (car ls) (lambda (b)
;;   >                          (filter-cps^ p (cdr ls) (if b
;;   >                                                      (lambda (ls^) (k (cons (car ls) ls^)))
;;   >                                                      k))))))))

;; ================================================================================

;; 10-2

;; > (define !-cps
;; >   (lambda (n k)
;; >     (if (zero? n)
;; >         (k 1)
;; >         (!-cps (sub1 n) (lambda (m)
;; >                           (k (* n m)))))))
;; >
;; > (define !-cps-cps
;; >   (lambda (n c k)
;; >     (if (zero? n)
;; >         (c 1 k)
;; >         (!-cps-cps (sub1 n)
;; >                    (lambda (n^ k)
;; >                      (c (* n n^) k))
;; >                    k))))
;; >
;; > (!-cps-cps 5 (lambda (x k) (k x)) (lambda (x) x))
;; - A CPSed interpreter:
;; > (define valof-cps
;; >   (lambda (exp env k)
;; >     (pmatch exp
;; >       (`,x (guard (symbol? x)) (env x k))
;; >       (`(lambda (,x) ,body) (k (lambda (a k) (valof-cps body (lambda (y k)
;; >                                                                (if (eqv? y x)
;; >                                                                    (k a)
;; >                                                                    (env y k)))
;; >                                                         k))))
;; >       (`(capture ,k^^ ,body) (valof-cps body (lambda (y k^)
;; >                                                (if (eqv? y k^^)
;; >                                                    (k^ k)
;; >                                                    (env y k^)))
;; >                                         k))
;; >       (`(return ,v-exp ,k-exp) (valof-cps k-exp env (lambda (k)
;; >                                                       (valof-cps v-exp env k))))
;; >       (`(,rator ,rand) (valof-cps rator env (lambda (rator^)
;; >                                               (valof-cps rand env (lambda (rand^)
;; >                                                                     (rator^ rand^ k)))))))))

;; ================================================================================

;; 9/7

;; - Representationally independent CPSed interpreter.
;; > (define extend-env
;; >   (lambda (env a x k)
;; >     (apply-k k (lambda (y k)
;; >                  (if (eqv? y x)
;; >                      (apply-k k a)
;; >                      (apply-env env y k))))))
;; >
;; > (define apply-env
;; >   (lambda (env y k)
;; >     (env y k)))
;; >
;; > (define empty-env
;; >   (lambda ()
;; >     (lambda (y k)
;; >       (apply-k k (error "unbound variable" y)))))
;; >
;; > (define closure
;; >   (lambda (x body env)
;; >     (lambda (a k)
;; >       (extend-env env a x (lambda (env)
;; >                             (valof body env k))))))
;; >
;; > (define apply-closure
;; >   (lambda (c a k)
;; >     (c a k)))
;; >
;; > (define empty-k
;; >   (lambda ()
;; >     (lambda (x) x)))
;; >
;; > (define apply-k
;; >   (lambda (k a)
;; >     (k a)))
;; > 
;; > (define inner-k
;; >   (lambda (closure k)
;; >     (lambda (arg)
;; >       (apply-closure closure arg k))))
;; >
;; > (define outer-k
;; >   (lambda (rand env k)
;; >     (lambda (closure)
;; >       (valof rand env (inner-k closure k)))))
;; >
;; > (define valof
;; >   (lambda (exp env k)
;; >     (pmatch exp
;; >       (`,x (guard (symbol? x)) (apply-env env x k))
;; >       ;; (`,n (guard (number? n)) (apply-k k n))
;; >       ;; (`(+ ,n1 ,n2) (valof n1 env (lambda (n1)
;; >       ;;                               (valof n2 env (lambda (n2)
;; >       ;;                                               (apply-k k (+ n1 n2)))))))
;; >       (`(lambda (,x) ,body) (apply-k k (closure x body env)))
;; >       (`(,rator ,rand) (valof rator env (outer-k rand env k))))))
;; - Changing to data structures:
;; > (define apply-k
;; >   (lambda (k v)
;; >     (pmatch k
;; >       (`(empty-k) v)
;; >       (`(inner-k ,closure ,k^) (apply-closure closure v k^))
;; >       (`(outer-k ,rand ,env ,k^) (valof rand env (inner-k v k^))))))
;; >
;; > (define inner-k
;; >   (lambda (closure k)
;; >     `(inner-k ,closure ,k)))
;; >
;; > (define outer-k
;; >   (lambda (rand env k)
;; >     `(outer-k ,rand ,env ,k)))
;; >
;; > (define empty-k
;; >   `(empty-k))

;; ================================================================================

;; 10-9

;; - Goal: translate Scheme to other languages (like C).
;; - CPS allows us to not worry about blowing the stack.
;; (define subst/?-cps
;;   (lambda (x e k)
;;     (cond
;;      ((pair? e) (subst/?-cps x (car e) (lambda (v)
;;                                          (subst/?-cps x (cdr e)
;;                                                       (lambda (u)
;;                                                         (k (cons v u)))))))
;;      ((eqv? e x) (k '?))
;;      (else (k e)))))

;; (define app-k-ri
;;   (lambda (k x)
;;     (k x)))

;; (define inner-k-ri
;;   (lambda (v k)
;;     (lambda (u)
;;       (app-k-ri k (cons v u)))))

;; (define outer-k-ri
;;   (lambda (x e k)
;;     (lambda (v)
;;       (subst/?-ri x (cdr e) (inner-k-ri v k)))))

;; (define empty-k-ri
;;   (lambda ()
;;     (lambda (x) x)))

;; (define subst/?-ri
;;   (lambda (x e k)
;;     (cond
;;      ((pair? e) (subst/?-ri x (car e) (outer-k-ri x e k)))
;;      ((eqv? e x) (app-k-ri k '?))
;;      (else (app-k-ri k e)))))

;; - And then we turn it to a data structure representation but I'm tired
;;   of typing/can't keep up with Jason.
;; - Then, convert to ANF: make sure that all tail calls are called with variables
;;   as arguments -- so, (f 4 (car l)) becomes (let ((x 4)) (let ((l (car l)) (f x l)))).
;; - Now we can turn every instance of let to set!
;; - This is registerization. All variables are in global registers, and we've
;;   taken all arguments out and reordered all set!s such that we don't accidentally overwrite
;;   a register whose value is needed.
;; - Now we can easily translate to C -- set! is variable assignment, function calls are gotos.

;; ================================================================================

;; 10-14

;; - Jason prohibited laptops...

;; ================================================================================

;; 10-16

(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (sub1 n))))))

(define fact-cps
  (lambda (n k)
    (if (zero? n)
        (k 1)
        (fact-cps (sub1 n)
                  (lambda (n^)
                    (k (* n n^)))))))

(define apply-k
  (lambda (k a)
    (k a)))

(define inner-k
  (lambda (n k)
    (lambda (n^)
      (apply-k k (* n n^)))))

(define fact-cps-ri
  (lambda (n k)
    (if (zero? n)
        (apply-k k 1)
        (fact-cps (sub1 n) (inner-k n k)))))

(define apply-k-ds
  (lambda (k v)
    (pmatch k
      (`(empty-k-ds) v)
      (`(inner-k-ds ,n^ ,k^) (let* ((k k^)
                                    (v (* n^ v)))
                               (apply-k-ds k v))))))

(define empty-k-ds
  (lambda ()
    `(empty-k-ds)))

(define inner-k-ds
  (lambda (n k)
    `(inner-k-ds ,n ,k)))

(define fact-cps-ri-ds
  (lambda (n k)
    (if (zero? n)
        (apply-k-ds k 1)
        (fact-cps-ri-ds (sub1 n) (inner-k-ds n k)))))

(define n 'foo)
(define k 'foo)
(define v 'foo)

;; (define fact-reg
;;   (lambda (n k)
;;     (if (zero? n)
;;         (let* ((v 1))
;;           (apply-k-ds k v))
;;         (let* ((k (inner-k-ds n k))
;;                (n (sub1 n)))
;;           (fact-reg n k)))))

(define apply-k-reg
  (lambda ()
    (pmatch k
      (`(empty-k-ds) v)
      (`(inner-k-ds ,n^ ,k^) (begin
                               (set! k k^)
                               (set! v (* n^ v))
                               (apply-k-reg))))))

(define fact-reg
  (lambda ()
    (if (zero? n)
        (begin
          (set! v 1)
          (apply-k-reg))
        (begin
          (set! k (inner-k-ds n k))
          (set! n (sub1 n))
          (fact-reg)))))

(define fact-driver
  (lambda ()
    (begin
      (set! n 5)
      (set! k (empty-k-ds))
      (fact-reg))))

;; This all comes from parentheC... just get this code later from oncourse

;; (define-union kt
;;   (empty-k)
;;   (fact-k n^ k^))

;; (define app-k
;;   (lambda ()
;;     (union-case k kt
;;       ((empty-k v) v)
;;       ((fact-k n^ k^) (begin
;;                         (set! k k^)
;;                         (set! v (* n n^)))))))

(define pc 'foo)

(define apply-k-tr
  (lambda ()
    (pmatch k
      (`(empty-k-ds) v)
      (`(inner-k-ds ,n^ ,k^) (begin
                               (set! k k^)
                               (set! v (* n^ n))
                               (set! pc apply-k-tr))))))

(define fact-reg-tr
  (lambda ()
    (if (zero? n)
        (begin
          (set! v 1)
          (set! pc (apply-k-tr)))
        (begin
          (set! k (inner-k-ds n k))
          (set! n (sub1 n))
          (set! pc fact-reg-tr)))))

(define tramp
  (lambda ()
    (begin
      (printf "~s\n" v)
      (pc)
      (tramp))))

(define fact-driver-tr
  (lambda ()
    (begin
      (set! n 5)
      (set! v 1)
      (set! k (empty-k-ds))
      (set! pc fact-reg-tr)
      (tramp))))

;; Arg, I fucked something up

;; ================================================================================

;; What if we want to do two operations on a list without passing over
;; it twice?

(require c311/let-pair)
(require c311/pmatch)

(define sumXmaxls
  (lambda (ls)
    (cond
     ((null? ls) '(0 . 0))
     (else (let-pair ((s . m) (sumXmaxls (cdr ls)))
             `(,(+ (car ls) s) . ,(max (car ls) m)))))))

(define fib
  (lambda (n s)
    (cond
     ((assv n s) => (lambda (pr) `(,(cdr pr) . ,s)))
     ((< n 2) `(,n . ((,n . ,n) . ,s)))
     (else (let-pair ((u . s^) (fib (- n 2) s))
             (let-pair ((v . s^^) (fib (sub1 n) s^))
               (let ((uv (+ u v)))
                 `(,uv ((,n . ,uv) . ,s^^)))))))))

;; Store-passing style

(define apply-env
  (lambda (env y)
    (cdr (assv env y))))

(define closure
  (lambda (x body env)
    `(closure ,x ,body ,env)))

(define apply-closure
  (lambda (clos arg s^^)
    (pmatch clos
      (`(closure ,x ,body ,env)
       (valof body `((,x . ,(length s^^)) . ,env) (append (s^^ (list arg))))))))

(define valof
  (lambda (exp env s)
    (pmatch exp
      (`(,x (guard (symbol? x))) (let ((addr (apply-env env x)))
                                   `(,(list-ref s addr) . ,s)))
      (`(lambda (,x) ,body) `(,(closure x body env) . ,s))
      (`(,rator ,rand) (let-pair ((arg . s^) (valof rand env s))
                         (let-pair ((clos . s^^) (valof rator env s^))
                           (apply-closure clos arg s^^)))))))

;; add set! at some point...

;; ================================================================================

;; Macros!

(define-syntax loop
  (syntax-rules ()
    ((loop) (loop))))

(define-syntax ifte
  (syntax-rules (then else)
    ((_ test then t else f) (if test t f))))

(define-syntax my-or
  (syntax-rules ()
    ((_) #f)
    ((_ b c ...) (let ((v b))
                   (if v
                       v
                       (my-or c ...))))))

(define-syntax my-let
  (syntax-rules ()
    ((_ ((var val) ...) body)
     ((lambda (var ...) body) val ...))))

(define-syntax my-let*
  (syntax-rules ()
    ((_ () body) body)
    ((_ ((var1 val1) (var2 val2) ...) body)
     ((lambda (var1)
        (my-let* ((var2 val2) ...)
          body)) val1))))

;; ================================================================================

;; Mini-Kanren

; Functions versus relations

(load "C311/mk.rkt")

(run 1 (q) (== 5 5))
;; (_.0)

(run 1 (q) (== 5 6))
;; ()

(run 1 (q) (== q 6))
;; (q)

(run 1 (q) (== q 6) (== q 7))
;; ()
;; Implicit conjunction of goals.

(run 1 (q)
  (fresh (a b)
    (conde
      ((== q `(,a ,b)))
      ((== a b)))))
;; ((_.0 _.1))

(define append
  (lambda (l s)
    (cond
     ((null? l) s)
     (else (cons (car l) (append (cdr l) s))))))

;; translating to mini kanren...

(define appendo
  (lambda (l s o)
    (conde
     ((== l '()) (== s o))
     ((=/= l '())
      (fresh (a d)
        (== `(,a . ,d) l)
        (fresh (res)
          (appendo d s res)
          (== o `(,a . ,res))))))))

(run 1 (q)
  (fresh (a b)
    (== a '(1 2 3))
    (== b '(4 5 6))
    (appendo a b q)))

;; ================================================================================

;; terms := sym | bool | term * term | vars
;; vars := Nat

(define var? number?)
(define var identity)

;; Substitutions
(define (ext-s x v s) `((,x . ,v) . ,x))

(define (walk u s)
  (let ((pr (assv u s)))
    (if pr
        (walk (cdr pr) s)
        u)))

(define (unify u v s)
  (let ((u (walk u s))
        (v (walk v s)))
    (cond
     ((eqv? u v) s)
     ((var? u) (ext-s u v s))
     ((var? v) (ext-s v u s))
     ((and (pair? u) (pair? v))
      (let ((s (unify (car u) (car v)) s))
        (and s (unify (cdr u) (cdr v)))))
     (else #f))))

(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s
          (list (cons s (cdr s/c)))
          '()))))

;; state ::= subst * counter
;; goal ::= state -> stream

;; call/fresh :: (var -> goal) -> goal
(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) `(,(car s/c) . ,(add1 c))))))

(define (disj g1 g2)
  (lambda (s/c)
    ($append (g1 s/c) (g2 s/c))))

(define ($append $1 $2)
  (cond
   ((null? $1) $2)
   ((pair? $1) (cons (car $1) ($append (cdr $1) $2)))))

(define (conj g1 g2)
  (lambda (s/c)
    ($append-map g2 (g1 s/c))))

(define ($append-map g $)
  (cond
   ((null? $) '())
   ((pair? $) ($append (g (car $)) ($append-map g (cdr $))))))

(define-syntax-rule (lambdac fs ge)
  (lambda (fs (lambda (s/c) (lambda (ge s/c))))))
