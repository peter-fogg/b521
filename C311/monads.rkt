#lang racket
(require C311/let-pair)
(provide (all-defined-out))

(define-syntax do
  (syntax-rules (<-)
    ((_ bind e) e)
    ((_ bind (v <- e) e* e** ...)
     (bind e (lambda (v) (do bind e* e** ...))))
    ((_ bind e e* e** ...)
     (bind e (lambda (_) (do bind e* e** ...))))))

(define return-maybe
  (lambda (a) `(Just ,a)))

(define bind-maybe
  (lambda (ma f)
    (cond
      [(eq? (car ma) 'Just) (f (cadr ma))]
      [(eq? (car ma) 'Nothing) '(Nothing)])))

(define fail
  (lambda ()
    '(Nothing)))

(define return-writer
  (lambda (a) `(,a . ())))

(define bind-writer
  (lambda (ma f)
    (let ([mb (f (car ma))])
      `(,(car mb) . ,(append (cdr ma) (cdr mb))))))

(define tell-writer
  (lambda (msg)
    `(__ . (,msg))))

(define return-state
  (lambda (a)
    (lambda (s)
      `(,a . ,s))))

(define bind-state
  (lambda (ma f)
    (lambda (s)
      (let ([vs^ (ma s)])
        (let ([v (car vs^)]
              [s^ (cdr vs^)])
          ((f v) s^))))))

(define get-state
  (lambda (s) `(,s . ,s)))

(define put-state
  (lambda (new-s)
    (lambda (s)
      `(__ . ,new-s))))

(define return-cont
  (lambda (a)
    (lambda (k)
      (k a))))

(define bind-cont
  (lambda (ma f)
    (lambda (k)
      (let ((k^ (lambda (a)
                  (let ((mb (f a)))
                    (mb k)))))
        (ma k^)))))

(define callcc
  (lambda (g)
    (lambda (k)
      (let ((k-as-proc (lambda (a) (lambda (k^) (k a)))))
        (let ((ma (g k-as-proc)))
          (ma k))))))
