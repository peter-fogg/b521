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
  (lambda (#:file-name (file "./a1.rkt")
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
  (test-suite "A1:"
    (test-suite "countdown"
      (test-equal-if-defined countdown
        ((countdown 5) '(5 4 3 2 1 0))))
    (test-suite "insertR"
      (test-equal-if-defined insertR
        ((insertR 'x 'y '(x z z x y x)) '(x y z z x y y x y))))
   ;; As with the original you may put as many tests as you would
   ;; like aftwards.
   (test-suite "insertR-fr" 
    (test-equal-if-defined insertR-fr
      ((insertR-fr 'x 'y '(x z z x y x)) '(x y z z x y y x y))
      ((insertR-fr 'x 'y '(x x x)) '(x y x y x y))))
   
   ;; And the most syntax sugar that I am going to offer test-equal-if-defined 
   ;; test-equal-if-defined is a macro expanding to check to see if this procedure
   ;; exists in the context of eval. If so the list following is tested for
   ;; equality the leftmost being tested with the sandboxed eval function.
   (test-suite "remv-1st"
     (test-equal-if-defined remv-1st
       [(remv-1st 'x '(x y z x)) '(y z x)]
       [(remv-1st 'y '(x y z y x)) '(x z y x)]))

   (test-suite "occurs-?s"
     (test-equal-if-defined occurs-?s
       [(occurs-?s '(? y z ? ?)) 3]))

   (test-suite "occurs-?s-fr"   
     (test-equal-if-defined occurs-?s-fr
       [(occurs-?s-fr '(? y z ? ?)) 3]))

   (test-suite "filter"   
     (test-equal-if-defined filter
       [(filter even? '(1 2 3 4 5 6)) '(2 4 6)]))

   (test-suite "filter-fr"   
     (test-equal-if-defined filter-fr
       [(filter even? '(1 2 3 4 5 6)) '(2 4 6)]))

   (test-suite "zip"   
     (test-equal-if-defined zip
       [(zip '(1 2 3) '(a b c)) '((1 . a) (2 . b) (3 . c))]))

   (test-suite "zip-fr"   
     (test-equal-if-defined zip-fr
       [(zip '(1 2 3) '(a b c)) '((1 . a) (2 . b) (3 . c))]))

   (test-suite "fact"   
     (test-equal-if-defined fact
       [(fact 5) 120]))

   (test-suite "fact-acc"   
     (test-equal-if-defined fact-acc
       [(fact 5) 120]))

   (test-suite "map"   
     (test-equal-if-defined map
       [(map add1 '(1 2 3 4)) '(2 3 4 5)]))

   (test-suite "map-fr"   
     (test-equal-if-defined map-fr
       [(map-fr add1 '(1 2 3 4)) '(2 3 4 5)]))

   (test-suite "append"   
     (test-equal-if-defined append
       [(append '(a b c) '(1 2 3)) '(a b c 1 2 3)]))

   (test-suite "append-fr"   
     (test-equal-if-defined append-fr
       [(append-fr '(a b c) '(1 2 3)) '(a b c 1 2 3)]))

   (test-suite "reverse"   
     (test-equal-if-defined reverse
       [(reverse '(a 3 x)) '(x 3 a)]))

   (test-suite "reverse-fr"   
     (test-equal-if-defined reverse-fr
       [(reverse-fr '(a 3 x)) '(x 3 a)]))

   (test-suite "member-?*"   
     (test-equal-if-defined member-?*
       [(member-?* '(a b c)) #f]
       [(member-?* '(a ? c)) #t]
       [(member-?* '((a ((?)) ((c) b c)))) #t]))

   (test-suite "fib"   
     (test-equal-if-defined fib
       [(fib 0) 0]
       [(fib 1) 1]
       [(fib 7) 13]))

   (test-suite "natural->binary"   
     (test-equal-if-defined natural->binary
       [(natural->binary 0) '()]
       [(natural->binary 4) '(0 0 1)]
       [(natural->binary 12) '(0 0 1 1)]
       [(natural->binary 15) '(1 1 1 1)]
       [(natural->binary 21) '(1 0 1 0 1)]
       [(natural->binary 8191) '(1 1 1 1 1 1 1 1 1 1 1 1 1)]))

   (test-suite "binary->natural"   
     (test-equal-if-defined binary->natural
       [(binary->natural '()) 0]
       [(binary->natural '(0 0 1)) 4]
       [(binary->natural '(0 0 1 1)) 12]
       [(binary->natural '(1 1 1 1)) 15]
       [(binary->natural '(1 0 1 0 1)) 21]
       [(binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191]))

   (test-suite "binary->natural-fr"   
     (test-equal-if-defined binary->natural-fr
       [(binary->natural-fr '()) 0]
       [(binary->natural-fr '(0 0 1)) 4]
       [(binary->natural-fr '(0 0 1 1)) 12]
       [(binary->natural-fr '(1 1 1 1)) 15]
       [(binary->natural-fr '(1 0 1 0 1)) 21]
       [(binary->natural-fr '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191]))

   (test-suite "power"   
     (test-equal-if-defined power
       [(power 2 0) 1]
       [(power 2 2) 4]
       [(power 2 10) 1024]
       [(power 10 5) 100000]
       [(power 3 31) 617673396283947]
       [(power 3 32) 1853020188851841]))

   (test-suite "minus" 
     (test-equal-if-defined minus
       [(minus 5 3) 2]
       [(minus 100 50) 50]))

   (test-suite "div"   
     (test-equal-if-defined div
       [(div 25 5) 5]
       [(div 36 6) 6]))

   (test-suite "collatz"   
     (test-equal-if-defined collatz
       [(collatz 12) 1]
       [(collatz 120) 1]
       [(collatz 9999) 1]))

   (test-suite "quine"   
     (test-equal-if-defined quine
       [(equal? (eval quine) quine) #t]
       [(equal? (eval quine) (eval (eval quine))) #t]))))
    
    
  

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

