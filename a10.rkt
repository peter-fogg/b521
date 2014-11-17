#lang racket

(require C311/mk)
(require C311/numbers)
(require C311/let-pair)

;; Part 1

;; 1. First miniKanren sees the constraint (== 5 q), so q shall now be
;; equal to 5. The first conde evaluates the second conde, which fails
;; because q is not == to both 5 and 6. The last line, (== q 5),
;; succeeds, and we get back '(5).

;; 2. We first create two fresh variables, a and b. The next line
;; constrains q to be a two-element list, where a is the first element
;; and b the second. The absento line tells us that 'tag is not an
;; element of q, and the last says that the first element of our
;; answer must not be a symbol. Our final answer is
;; '(((_.0 _.1) (=/= ((_.0 tag))) (sym _.0) (absento (tag _.1)))).

;; 3a. The argument is not a pair.
;; 3b. The two arguments are not equal.
;; 3c. The first argument is not an element of the second.
;; 3d. The argument is a number.
;; 3e. The argument is a symbol.

;; Part 2

(define assoco
  (lambda (x ls o)
    (fresh (a d aa da)
      (== `(,a . ,d) ls)
      (== `(,aa . ,da) a)
      (conde
        ((== aa x) (== o a))
        ((=/= a x) (assoco x d o))))))

(define reverseo
  (lambda (ls o)
    (conde
      ((== ls '()) (== o '()))
      ((=/= ls '())
         (fresh (a d res)
           (== `(,a . ,d) ls)
           (reverseo d res)
           (appendo res `(,a) o))))))

(define stuttero
  (lambda (ls o)
    (conde
      ((== ls '()) (== o '()))
      ((=/= ls '())
       (fresh (a d res)
         (== `(,a . ,d) ls)
         (== `(,a ,a . ,res) o)
         (stuttero d res))))))

;; Brainteaser

(define lengtho
  (lambda (l o)
    (conde
      ((== l '()) (== o (build-num 0)))
      ((=/= l '())
       (fresh (a d res)
         (== l `(,a . ,d))
         (lengtho d res)
         (pluso (build-num 1) res o))))))
