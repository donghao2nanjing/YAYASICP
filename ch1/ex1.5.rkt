#lang sicp

(define (p) (p))

(define (test x y) 
    (if (= x 0) 0 y))

;;; applicative-order evaluation: the interpreter does not return.
;;; reference: https://sukritkalra94.wordpress.com/2014/05/13/sicp-exercise-1-5-ben-bitdiddles-test/
(test 0 (p))
