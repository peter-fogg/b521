#lang racket
;; C311 Assignment 1 test suite
;; Written in Spring 2010 by Lindsey Kuper
;; Updated for Spring 2012 by Ross Larson
;; Updated for Fall 2013 by Jason Hemann
;; Updated for Spring 2014 by Andre Kuhlenschmidt
;; Updated for Fall 2014 by Jason Hemann
#| The underlying principles of the autograding framework is simple. 
We use the rackunit unit testing framework that comes with the racket
distrobution. We have define a set of calls that take a test-suite.
and executes the test-suite with eval rebound to a sandboxed evaluator
that provides racket and the file being tested. Disk access should be
limited to those files and time and space are limited by the parameters
time-for-eval and space-for-eval. 
|#

(require rackunit rackunit/text-ui racket/sandbox)
(provide test-file)

#|
 Test File is the minimum requirement for being able to understand
 our the autograder. test-file is a function that when invoked with
 no arguments will search will the current directory for the file 
 named a1.rkt and run the test suite with that file.
 If a single argument is provided that argument must be the relative or
 absolute path to the file containing the definitions for the assignment.
|#

(define test-file
  (lambda (#:file-name (file "./a2.rkt")
	   #:sec-of-eval (sec 5)
	   #:mb-of-eval (mb 5))
    (parameterize ((read-accept-reader #t)
                   (read-accept-lang #t))
      (let ((sandboxed-eval
             (make-module-evaluator (read (open-input-file file))
                                    #:allow-for-require '(C311/pmatch))))
        (set-eval-limits sandboxed-eval sec mb)
        (parameterize ((current-eval sandboxed-eval)
	               (error-print-context-length 0))
          (run-tests tests))))))

#|
  A tests is the name of the test-suite that is run by a call to test-file.
The test suite is a type that is define in the rackunit module. I will give
some examples of how test might be structured. If furthure documentation is
require feel free to browse the rackunit documentation at the following address.
http://docs.racket-lang.org/rackunit/?q=rackunit
|#
    
(define tests
(test-suite "A2:"
  (test-suite "list-ref" 
    (test-equal-if-defined list-ref
      ((list-ref '(a b c) 2) 'c)
      ((list-ref '(a b c) 0) 'a)))

  (test-suite "union" 
    (test-equal-if-defined union
      ((union '() '()) '())
      ((union '(x) '()) '(x))
      ((union '(x) '(x)) '(x))
      ((union '(x y) '(x z)) '(x y z))))

  (test-suite "extend" 
    (test-equal-if-defined extend
      (((extend 1 even?) 0) '#t)
      (((extend 1 even?) 1) '#t)
      (((extend 1 even?) 2) '#t)
      (((extend 1 even?) 3) '#f)
      ((filter (extend 1 even?) '(0 1 2 3 4 5)) '(0 1 2 4))
      ((filter (extend 3 (extend 1 even?)) '(0 1 2 3 4 5)) '(0 1 2 3 4))
      ((filter (extend 7 (extend 3 (extend 1 even?))) '(0 1 2 3 4 5)) '(0 1 2 3 4))))

  (test-suite "walk-symbol" 
    (test-equal-if-defined walk-symbol
      ((walk-symbol 'a '((a . 5))) '5)
      ((walk-symbol 'a '((b . c) (a . b))) 'c)
      ((walk-symbol 'a '((a . 5) (b . 6) (c . a))) '5)
      ((walk-symbol 'c '((a . 5) (b . (a . c)) (c . a))) '5)
      ((walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a))) '((c . a)))
      ((walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e))) '5)
      ((walk-symbol 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e))) 'f)))

  (test-suite "lambda-lumbda'" 
    (test-equal-if-defined lambda->lumbda
      ((lambda->lumbda 'x) 'x)
      ((lambda->lumbda '(lambda (x) x)) '(lumbda (x) x))
      ((lambda->lumbda '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a)))))) '(lumbda (z) ((lumbda (y) (a z)) (h (lumbda (x) (h a))))))
      ((lambda->lumbda '(lambda (lambda) lambda)) '(lumbda (lambda) lambda))
      ((lambda->lumbda '((lambda (lambda) lambda) (lambda (y) y))) '((lumbda (lambda) lambda) (lumbda (y) y)))))

  (test-suite "vars" 
    (test-equal-if-defined vars
      ((vars 'x) '(x))
      ((vars '(lambda (x) x)) '(x))
      ((vars '((lambda (y) (x x)) (x y))) '(x x x y))
      ((vars '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a)))))) '(a z h h a))))

  (test-suite "unique-vars" 
    (test-equal-if-defined unique-vars
      ((unique-vars '((lambda (y) (x x)) (x y))) '(x y))
      ((unique-vars '((lambda (z) (lambda (y) (z y))) x)) '(z y x))
      ((unique-vars '((lambda (a) (a b)) ((lambda (c) (a c)) (b a)))) '(c b a))))

  (test-suite "var-occurs-free?" 
    (test-equal-if-defined var-occurs-free?
      ((var-occurs-free? 'x 'x) '#t)
      ((var-occurs-free? 'x '(lambda (y) y)) '#f)
      ((var-occurs-free? 'x '(lambda (x) (x y))) '#f)
      ((var-occurs-free? 'y '(lambda (x) (x y))) '#t)
      ((var-occurs-free? 'y '((lambda (y) (x y)) (lambda (x) (x y)))) '#t)
      ((var-occurs-free? 'x '((lambda (x) (x x)) (x x))) '#t)))

  (test-suite "var-occurs-bound?" 
    (test-equal-if-defined var-occurs-bound?
      ((var-occurs-bound? 'x 'x) '#f)
      ((var-occurs-bound? 'x '(lambda (x) x)) '#t)
      ((var-occurs-bound? 'y '(lambda (x) x)) '#f)
      ((var-occurs-bound? 'x '((lambda (x) (x x)) (x x))) '#t)
      ((var-occurs-bound? 'z '(lambda (y) (lambda (x) (y z)))) '#f)
      ((var-occurs-bound? 'z '(lambda (y) (lambda (z) (y z)))) '#t)
      ((var-occurs-bound? 'x '(lambda (x) y)) '#f)
      ((var-occurs-bound? 'x '(lambda (x) (lambda (x) x))) '#t)))

  (test-suite "unique-free-vars" 
    (test-equal-if-defined unique-free-vars
      ((unique-free-vars 'x) '(x))
      ((unique-free-vars '(lambda (x) (x y))) '(y))
      ((unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c))))))) '(y e x))))

  (test-suite "unique-bound-vars" 
    (test-equal-if-defined unique-bound-vars
      ((unique-bound-vars 'x) '())
      ((unique-bound-vars '(lambda (x) (x y))) '(x))
      ((unique-bound-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c))))))) '(x c))
      ((unique-bound-vars '(lambda (x) y)) '())
      ((unique-bound-vars '(lambda (x) (y z))) '()) ))

  (test-suite "lex" 
    (test-equal-if-defined lex
    ((lex 'x '()) '(free-var x))
    ((lex '(lambda (x) x) '()) '(lambda (var 0)))
    ((lex '(lambda (x) y) '()) '(lambda (free-var y)))
    ((lex '(lambda (x) (x y)) '()) '(lambda ((var 0) (free-var y))))
    ((lex '((lambda (x) (x y)) (lambda (c) (lambda (d) (e c)))) '()) '((lambda ((var 0) (free-var y))) (lambda (lambda ((free-var e) (var 1))))))
((lex '(lambda (a)
          (lambda (b)
            (lambda (c)
              (lambda (a)
                (lambda (b)
                  (lambda (d)
                    (lambda (a)
                      (lambda (e)
                        (((((a b) c) d) e) f))))))))) '()) '(lambda
  (lambda
    (lambda
      (lambda
        (lambda
          (lambda
            (lambda
              (lambda
                ((((((var 1) (var 3)) (var 5)) (var 2)) (var 0)) (free-var f)))))))))))
((lex '((lambda (a)
           (lambda (b)
             (lambda (c)
               (((((a b) c) w) x) y))))
         (lambda (w)
           (lambda (x)
             (lambda (y)
               (((((a b) c) w) x) y))))) '()) '((lambda
   (lambda
     (lambda
       ((((((var 2) (var 1)) (var 0)) (free-var w)) (free-var x)) (free-var y)))))
 (lambda
   (lambda
     (lambda
       ((((((free-var a) (free-var b)) (free-var c)) (var 2)) (var 1)) (var 0)))))))))

  (test-suite "unify" 
    (test-equal-if-defined unify
      ((unify 'x 'y '()) '((x . y))) ;; Another correct answer: ((y . x)) 
      ((unify '(x) '(y) '()) '((x . y))) ;; Another correct answer: ((y . x)) 
      ((unify 5 5 '()) '())
      ((unify 5 6 '()) '#f)
      ((unify '(5 6) '(x y) '()) '((x . 5) (y . 6))) ;; Another correct answer: ((y . 6) (x . 5))
      ((unify '(z 5) '(5 x) '((z . 3) (x . z))) '#f)
      ((unify '((x . 5) (y . z)) '((y . 5) (x . 5)) '((z . 5) (x . y))) '((z . 5) (x . y))))) ;; Another correct answer: ((x . y) (z . 5))

  (test-suite "walk-symbol-update" 
    (test-equal-if-defined walk-symbol-update
      ((let ((a-list `((c . ,(box 15)) (e . ,(box 'f)) (b . ,(box 'c)) (a . ,(box 'b)))))
         (walk-symbol-update 'a a-list))
       '15)))       

  (test-suite "var-occurs-both?" 
    (test-equal-if-defined var-occurs-both?
      ((var-occurs-both? 'x '(lambda (x) (x (lambda (x) x)))) '#f) 
      ((var-occurs-both? 'x '(x (lambda (x) x))) '#t)
      ((var-occurs-both? 'x '(lambda (y) (x (lambda (x) x)))) '#t)
      ((var-occurs-both? 'x '(lambda (x) (lambda (x) (x (lambda (x) x))))) '#f)
      ((var-occurs-both? 'x '(lambda (x) (lambda (y) (lambda (x) (x (lambda (x) x)))))) '#f)
      ((var-occurs-both? 'x '(lambda (y) (lambda (x) (lambda (z) (lambda (x) (x (lambda (x) x))))))) '#f)))))
    
  

(define-syntax test-if-defined
  (syntax-rules ()
    ((_ sym tests ...)
     (test-case (format "~a undefined" 'sym)
                (check-not-false (lambda () (eval 'sym)))
                tests ...))))

(define-syntax test-equal-if-defined
  (syntax-rules ()
    ((_ ident (expr val) ...)
      (let ((n 1))
        (test-case (format "~a: undefined" 'ident)
                   (check-not-exn (lambda () (eval 'ident)))
                   (test-case (format "~a: ~a" 'ident n)
                              (with-check-info 
                               (('tested 'expr))
                               (set! n (add1 n))
                               (check equal? (eval 'expr) val))) ...)))))

(define-syntax ifdef-suite
  (syntax-rules ()
    ((_ ident (expr val) ...)
     (let ((n 1))
       (test-suite (~a 'ident)
        (test-case "undefined"
         (check-not-exn (lambda () (eval 'ident)))
         (test-case (~a n)
          (with-check-info (('tested 'expr))
           (set! n (add1 n))
           (check equal? (eval 'expr) val))) ...))))))

