#lang racket

(require c311/pmatch)

(define empty-k
  (lambda (jumpout)
   `(empty-k ,jumpout)))

(define loop
  (lambda (th)
    (loop (th))))

;; Ack, trampoline

(define ack-k-tramp
  (lambda (m k)
    `(ack-k-tramp ,m ,k)))

(define app-k-ack-tramp
  (lambda (k v)
    (pmatch k
      (`(empty-k ,jumpout) (jumpout v))
      (`(ack-k-tramp ,m ,k) (lambda () (ack-tramp (sub1 m) v k))))))

(define ack-tramp
  (lambda (m n k)
    (cond
     ((zero? m) (lambda () (app-k-ack-tramp k (add1 n))))
     ((zero? n) (lambda () (ack-tramp (sub1 m) 1 k)))
     (else (lambda () (ack-tramp m (sub1 n) (ack-k-tramp m k)))))))

(define ack-tramp-driver
  (lambda (m n)
    (call/cc (lambda (k)
               (loop (lambda () (ack-tramp m n (empty-k k))))))))

;; Depth, trampoline

(define inner-depth-k-tramp
  (lambda (l k)
    `(inner-depth-k-tramp ,l ,k)))

(define outer-depth-k-tramp
  (lambda (ls k)
    `(outer-depth-k-tramp ,ls ,k)))

(define app-k-depth-tramp
  (lambda (k v)
    (pmatch k
      (`(empty-k ,jumpout) (jumpout v))
      (`(inner-depth-k-tramp ,l ,k) (let ((l (add1 l)))
                                      (lambda () (app-k-depth-tramp k (if (< l v) v l)))))
      (`(outer-depth-k-tramp ,ls ,k) (lambda () (depth-tramp (cdr ls) (inner-depth-k-tramp v k)))))))

(define depth-tramp
  (lambda (ls k)
    (cond
     ((null? ls) (lambda () (app-k-depth-tramp k 1)))
     ((pair? (car ls)) (lambda () (depth-tramp (car ls) (outer-depth-k-tramp ls k))))
     (else (lambda () (depth-tramp (cdr ls) k))))))

(define depth-tramp-driver
  (lambda (ls)
    (call/cc (lambda (k)
               (loop (depth-tramp ls (empty-k k)))))))

;; Fact, trampoline

(define fact-k^
  (lambda (k n)
    `(fact-k^ ,k ,n)))

(define app-k-fact-tramp
  (lambda (k v)
    (pmatch k
      (`(empty-k ,jumpout) (jumpout v))
      (`(fact-k^ ,k ,n) (lambda () (app-k-fact-tramp k (* n v)))))))

(define fact-tramp
  (lambda (n k)
    ((lambda (fact-tramp k)
       (lambda () (fact-tramp fact-tramp n k)))
     (lambda (fact-tramp n k)
       (cond
        ((zero? n) (lambda () (app-k-fact-tramp k 1)))
        (else (lambda () (fact-tramp fact-tramp (sub1 n) (fact-k^ k n))))))
     k)))

(define fact-tramp-driver
  (lambda (n)
    (call/cc (lambda (k)
               (loop (fact-tramp n (empty-k k)))))))

;; Pascal, trampoline

(define inner-pascal-k
  (lambda (k a)
    `(inner-pascal-k ,a ,k)))

(define outer-pascal-k
  (lambda (m a k)
    `(outer-pascal-k ,m ,a ,k)))

(define init-pascal-k
  (lambda (k)
    `(init-pascal-k ,k)))

(define app-k-pascal-tramp
  (lambda (k v)
    (pmatch k
      (`(empty-k ,jumpout) (jumpout v))
      (`(inner-pascal-k ,k ,a) (lambda () (app-k-pascal-tramp k (cons a v))))
      (`(outer-pascal-k ,m ,a ,k) (lambda () (v (add1 m) a (inner-pascal-k a k))))
      (`(init-pascal-k ,k) (lambda () (v 1 0 k))))))

(define pascal-tramp
  (lambda (n k)
    (let ((pascal-tramp
           (lambda (pascal-tramp k)
             (lambda () (app-k-pascal-tramp
                         k (lambda (m a k)
                             (cond
                              ((> m n) (app-k-pascal-tramp k '()))
                              (else (let ((a (+ a m)))
                                      (pascal-tramp pascal-tramp (outer-pascal-k m a k)))))))))))
      (lambda () (pascal-tramp pascal-tramp (init-pascal-k k))))))

(define pascal-tramp-driver
  (lambda (n)
    (call/cc (lambda (k)
               (loop (pascal-tramp n (empty-k k)))))))

;; Part II

(define empty-k-reg
  (lambda ()
    `(empty-k-reg)))

;; Ack, registerized

(define ack-m 'foo)
(define ack-n 'foo)
(define ack-k 'foo)
(define ack-v 'foo)

(define ack-k-reg
  (lambda (m k)
    `(ack-k-reg ,m ,k)))

(define app-k-ack-reg
  (lambda () ;; k v
    (pmatch ack-k
      (`(empty-k-reg) ack-v)
      (`(ack-k-reg ,m ,k) (begin
                            (set! ack-m (sub1 m))
                            (set! ack-n ack-v)
                            (set! ack-k k)
                            (ack-reg))))))

(define ack-reg
  (lambda () ;; m n k
    (cond
     ((zero? ack-m) (begin
                  (set! ack-v (add1 ack-n))
                  (app-k-ack-reg)))
     ((zero? ack-n) (begin
                  (set! ack-m (sub1 ack-m))
                  (set! ack-n 1)
                  (ack-reg)))
     (else (begin
             (set! ack-n (sub1 ack-n))
             (set! ack-k (ack-k-reg ack-m ack-k))
             (ack-reg))))))

(define ack-reg-driver
  (lambda (m n)
    (begin
      (set! ack-m m)
      (set! ack-n n)
      (set! ack-k (empty-k-reg))
      (ack-reg))))

;; Depth, registerized

(define depth-v 'foo)
(define depth-ls 'foo)
(define depth-l 'foo)
(define depth-k 'foo)

(define inner-depth-k-reg
  (lambda (l k)
    `(inner-depth-k-reg ,l ,k)))

(define outer-depth-k-reg
  (lambda (ls k)
    `(outer-depth-k-reg ,ls ,k)))

(define app-k-depth-reg
  (lambda () ;; k v
    (pmatch depth-k
      (`(empty-k-reg) depth-v)
      (`(inner-depth-k-reg ,l ,k) (begin
                                    (set! depth-l (add1 l))
                                    (set! depth-v (if (< depth-l depth-v) depth-v depth-l))
                                    (set! depth-k k)
                                    (app-k-depth-reg)))
      (`(outer-depth-k-reg ,ls ,k) (begin
                                     (set! depth-ls (cdr ls))
                                     (set! depth-k (inner-depth-k-reg depth-v k))
                                     (depth-reg))))))

(define depth-reg
  (lambda () ;; ls k
    (cond
     ((null? depth-ls) (begin
                   (set! depth-v 1)
                   (app-k-depth-reg)))
     ((pair? (car depth-ls)) (begin
                               (set! depth-k (outer-depth-k-reg depth-ls depth-k))
                               (set! depth-ls (car depth-ls))
                               (depth-reg)))
     (else (begin
             (set! depth-ls (cdr depth-ls))
             (depth-reg))))))

(define depth-reg-driver
  (lambda (ls)
    (begin
      (set! depth-ls ls)
      (set! depth-k (empty-k-reg))
      (depth-reg))))

;; Fact, registerized

(define fact-k 'foo)
(define fact-n 'foo)
(define fact-v 'foo)
(define fact-fact-reg 'foo)
(define fact-rand 'foo)

(define fact-k-reg
  (lambda (k n)
    `(fact-k-reg ,k ,n)))

(define app-k-fact-reg
  (lambda () ;; k v
    (pmatch fact-k
      (`(empty-k-reg) fact-v)
      (`(fact-k-reg ,k ,n) (begin
                             (set! fact-v (* n fact-v))
                             (set! fact-k k)
                             (app-k-fact-reg))))))

(define fact-reg
  (lambda () ;; n k
    (begin
      (set! fact-rand (lambda ()
                        (cond
                         ((zero? fact-n) (begin
                                           (set! fact-v 1)
                                           (app-k-fact-reg)))
                         (else (begin
                                 (set! fact-k (fact-k-reg fact-k fact-n))
                                 (set! fact-n (sub1 fact-n))
                                 (fact-rand))))))
      (fact-rand))))

(define fact-reg-driver
  (lambda (n)
    (set! fact-n n)
    (set! fact-k (empty-k-reg))
    (fact-reg)))

;; Pascal, registerized

(define pascal-pascal-reg 'foo)
(define pascal-k 'foo)
(define pascal-n 'foo)
(define pascal-m 'foo)
(define pascal-a 'foo)
(define pascal-v 'foo)

(define inner-pascal-k-reg
  (lambda (k a)
    `(inner-pascal-k-reg ,a ,k)))

(define outer-pascal-k-reg
  (lambda (m a k)
    `(outer-pascal-k-reg ,m ,a ,k)))

(define init-pascal-k-reg
  (lambda (k)
    `(init-pascal-k-reg ,k)))

(define app-k-pascal-reg
  (lambda () ;; k v
    (pmatch pascal-k
      (`(empty-k-reg) pascal-v)
      (`(inner-pascal-k-reg ,k ,a) (begin
                                     (set! pascal-v (cons a pascal-v))
                                     (set! pascal-k k)
                                     (app-k-pascal-reg)))
      (`(outer-pascal-k-reg ,m ,a ,k) (begin
                                        (set! pascal-m (add1 m))
                                        (set! pascal-k (inner-pascal-k-reg a k))
                                        (pascal-v)))
      (`(init-pascal-k-reg ,k) (begin
                                 (set! pascal-m 1)
                                 (set! pascal-a 0)
                                 (set! pascal-k k)
                                 (pascal-v))))))

(define pascal-reg
  (lambda () ;; n k
    (begin
      (set! pascal-reg
       (lambda () ;; pascal-reg k
         (begin
           (set! pascal-v (lambda () ;; m a k
                            (cond
                             ((> pascal-m pascal-n) (begin
                                                      (set! pascal-v '())
                                                      (app-k-pascal-reg)))
                             (else (begin
                                     (set! pascal-a (+ pascal-a pascal-m))
                                     (set! pascal-k (outer-pascal-k-reg pascal-m pascal-a pascal-k))
                                     (pascal-reg))))))
           (app-k-pascal-reg))))
      (set! pascal-k (init-pascal-k-reg pascal-k))
      (pascal-reg))))

(define pascal-reg-driver
  (lambda (n)
    (begin
      (set! pascal-n n)
      (set! pascal-k (empty-k-reg))
      (pascal-reg))))

;; Brainteaser

(define inner-fib-k
  (lambda (n k)
    `(inner-fib-k ,n ,k)))

(define outer-fib-k
  (lambda (n k)
    `(outer-fib-k ,n ,k)))

(define app-k-fib-tramp
  (lambda (k v)
    (pmatch k
      (`(empty-k ,jumpout) (jumpout v))
      (`(inner-fib-k ,n ,k) (lambda () (app-k-fib-tramp k (+ n v))))
      (`(outer-fib-k ,n ,k) (lambda () (fib (- n 2) (inner-fib-k v k)))))))

(define fib
  (lambda (n k)
    (cond
     ((= n 0) (lambda () (app-k-fib-tramp k 1)))
     ((= n 1) (lambda () (app-k-fib-tramp k 1)))
     (else (lambda () (fib (sub1 n) (outer-fib-k n k)))))))

(define shuffle
  (lambda (l)
    (if (null? l)
        '()
        (let* ((len (length l))
               (r (random len))
               (e (list-ref l r))
               (init (take l r))
               (rest (drop l (add1 r))))
          (cons e (shuffle (append init rest)))))))

(define rampoline
  (lambda thunks
    (let ((thunk (list-ref thunks (random (length thunks)))))
      (apply rampoline (cons (thunk) (shuffle (remove thunk thunks)))))))

(define fib-dr
  (lambda (n1 n2 n3)
    (call/cc (lambda (jumpout)
               (rampoline
                (lambda () (fib n1 (empty-k jumpout)))
                (lambda () (fib n2 (empty-k jumpout)))
                (lambda () (fib n3 (empty-k jumpout))))))))

;; Just Dessert

;; (define bi-trampoline
;;   (lambda (th1 th2)
;;     (bi-trampoline (th2) th1)))

;; (list
;;  (call/cc
;;   (lambda (k)
;;     (bi-trampoline (lambda () (fib 10 (empty-k (lambda (v)
;;                                                  (begin
;;                                                    ()
;;                                                    (k v))))))
;;                    (lambda () (fib 1 (empty-k k)))))))

;; (let ((l '())
;;       (r #f)
;;       (kont #f))
;;   (begin
;;     (set! r (call/cc (lambda (k)
;;                        (begin
;;                          (set! kont k)
;;                          (bi-trampoline (lambda () (fib 1 (empty-k kont)))
;;                                         (lambda () (fib 10 (empty-k kont))))))))
;;     (set! kont (call/cc identity))
;;     (print "foo")
;;     (set! l (cons r l))
;;     (list r l)))

;; (call/cc
;;  (lambda (k)
;;    (letrec ((used #f)
;;             (cont (lambda (v)
;;                     (if used

;;                         (begin
;;                           (set! used #t)
;;                           )))))
;;      )))
