#lang sicp 

(define x (cons (list 1 2) (list 3 4)))

(display "test x:")
(newline)
(length x)
(car x)
(cdr x)
(length (cdr x))
(newline)

;;; Turn to page 135 to see the definition of procedure list.
(display "test (list x x):")
(newline)
(length (list x x))
(car (list x x))
(cdr (list x x))
(length (cdr (list x x)))
(newline)

(display "test (cons x x):")
(newline)
(length (cons x x))
(car (cons x x))
(cdr (cons x x))
(length (cdr (cons x x)))
(newline)

(display "(list 1 (list 2 (list 3 4)))")
(newline)
(list 1 (list 2 (list 3 4)))