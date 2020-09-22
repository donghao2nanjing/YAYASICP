#lang sicp

;;;That is a + |b|
(define (a-plus-abs-b a b) 
    ((if (> b 0) + - ) a b))

(a-plus-abs-b 1 2)
(a-plus-abs-b 1 -2)