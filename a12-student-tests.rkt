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
  (lambda (#:file-name (file "./a12.rkt")
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
  (test-suite "a12"
    (test-suite "assv-maybe"
      (test-equal-if-defined assv-maybe
	((assv-maybe 'c '((a . 1) (b . 2) (c . 3))) '(Just 3))
	((assv-maybe 'd '((a . 1) (b . 2) (c . 3))) '(Nothing))))
    (test-suite "partition-writer"
      (test-equal-if-defined partition-writer
	((partition-writer even? '(1 2 3 4 5 6 7 8 9 10))
	 '((1 3 5 7 9) . (2 4 6 8 10)))
	((partition-writer odd? '(1 2 3 4 5 6 7 8 9 10))
	 '((2 4 6 8 10) . (1 3 5 7 9)))))
    (test-suite "powerXpartials"
      (test-equal-if-defined powerXpartials
	((powerXpartials 2 6) '(64 . (2 4 8)))
	((powerXpartials 3 5) '(243 . (3 9 81)))
	((powerXpartials 5 7) '(78125 . (5 25 125 15625)))))
    (test-suite "abc-game"
      (test-equal-if-defined abc-game
	(((abc-game '(a b c c b a)) 0) '(__ . 0))
	(((abc-game '(a b c c b a a)) 0) '(__ . 1))
	(((abc-game '(a a a)) 0) '(__ . 3))))
    (test-suite "reciprocal"
      (test-equal-if-defined reciprocal			       
        ((reciprocal 0) '(Nothing))
	((reciprocal 2) '(Just 1/2))
	((traverse-reciprocal '((1 . 2) . (3 . (4 . 5)))) 
	 '(Just ((1 . 1/2) . (1/3 . (1/4 . 1/5)))))
	((traverse-reciprocal '((1 . 2) . (0 . (4 . 5))))
	 '(Nothing))))
    (test-suite "halve"
      (test-equal-if-defined halve
        ((halve 6) '(3 . ()))
	((halve 5) '(5 . (5)))
	((traverse-halve '((1 . 2) . (3 . (4 . 5))))
	 '(((1 . 1) . (3 . (2 . 5))) . (1 3 5)))))
    (test-suite "state/sum"
      (test-equal-if-defined state/sum
        (((state/sum 5) 0) '(0 . 5))
	(((state/sum 2) 0) '(0 . 2))
	(((state/sum 2) 3) '(3 . 5))
	(((traverse-state/sum '((1 . 2) . (3 . (4 . 5)))) 0)
	 '(((0 . 1) 3 6 . 10) . 15))))
    (test-suite "value-of-cps"
      (test-equal-if-defined value-of-cps
	(((value-of-cps fact-5 (empty-env)) (lambda (v) v)) '120) 
	(((value-of-cps capture-fun (empty-env)) (lambda (v) v)) '12)))
    (test-suite "same-fringe"
      (test-equal-if-defined yield-cont
	((driver '(("Time" . "flies") . ("like" . ("an" . "arrow")))
		 '("time" . ("FLIES" . (("like" . "an") . "aRrOw")))) 
	 '((("time" . "FLIES") . ("like" . ("an" . "aRrOw")))
	   ("Time" . ("flies" . (("like" . "an") . "arrow")))))
	((driver '(("Time" . "flies") . ("like" . ("arrow" . "an")))
		 '("time" . ("FLIES" . (("like" . "an") . "aRrOw")))) 
	 '#f)))))


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

