#lang sicp 

;;; iteration implememtation.
(define (f n) 
    (if (< n 3) n 
    (f_iter 0 1 2 n)
    )
)

(define (f_iter c b a count) 
    (if (= count 3) (+ a (* 2 b) (* 3 c)) 
    (f_iter b a  (+ a (* 2 b) (* 3 c)) (- count 1))
    )
)

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
