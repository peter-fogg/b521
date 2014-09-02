#lang racket
(require rnrs/io/ports-6)
(provide (all-defined-out))

(define sort-list-of-symbols
  (lambda (ls)
    (sort ls lex<=?)))

(define sort-unify-result
  (lambda (ls)
    (let ((sls (map (lambda (e) (let ((a (car e))
				 (d (cdr e)))
			     (if (and (symbol? d) (lex<=? d a))
				 `(,d . ,a)
				 e)))
		    ls)))
      (sort sls lex<=?))))

(define lex<=?
  (lambda (x y)
    (string<=? (datum->string x) (datum->string y))))

(define datum->string
  (lambda (x)
    (call-with-string-output-port
      (lambda (p) (display x p)))))
