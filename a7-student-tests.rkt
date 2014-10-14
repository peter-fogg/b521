#lang racket
;; Written for Spring 2013 by Andre Kuhlenschmidt and Jason Hemann

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
  (lambda (#:file-name (file "./a7.rkt")
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
  (test-suite "a7"

    (test-suite "value-of-cps"
      (test-equal-if-defined value-of-cps
        ((value-of-cps '((lambda (f)
			   ((f f) 5))
			 (lambda (f)
			   (lambda (n)
			     (if (zero? n)
				 1
				 (* n ((f f) (sub1 n))))))) (empty-env) (empty-k))
	 120)

	((value-of-cps '(* 3 (capture q (* 2 (return 4 q)))) (empty-env) (empty-k))
	 12)))
    (test-suite "value-of-cps-fn"
      (test-equal-if-defined value-of-cps-fn
	((value-of-cps-fn '((lambda (f)
			      ((f f) 5))
			    (lambda (f)
			      (lambda (n)
				(if (zero? n)
				    1
				    (* n ((f f) (sub1 n))))))) (empty-env) (empty-k-fn))
	 120)

	((value-of-cps-fn '(* 3 (capture q (* 2 (return 4 q)))) (empty-env) (empty-k-fn))
	 12)))
    (test-suite "value-of-cps-ds"
      (test-equal-if-defined value-of-cps-ds
	((value-of-cps-ds '((lambda (f)
			      ((f f) 5))
			    (lambda (f)
			      (lambda (n)
				(if (zero? n)
				    1
				    (* n ((f f) (sub1 n))))))) (empty-env) (empty-k-ds))
	 120)

	((value-of-cps-ds '(* 3 (capture q (* 2 (return 4 q)))) (empty-env) (empty-k-ds))
	 12)))
    (test-suite "trib$"
      (test-equal-if-defined trib$
	((car$ trib$) 0)
	((car$ (cdr$ trib$)) 1)
	((take$ 7 trib$) '(0 1 1 2 4 7 13))))))

;; Copied over from a6

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
