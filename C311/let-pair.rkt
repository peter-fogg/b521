#lang racket
(provide let-pair)
(define-syntax let-pair
  (syntax-rules ()
    [(_ ((a . s) call) body)
      (let ((tmp-res call))
        (let ((a (car tmp-res))
               (s (cdr tmp-res)))
          body))]))
