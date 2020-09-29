#lang sicp 

(define (accumulate op init seq)
    (if (null? seq)
        init
        (op (car seq) (accumulate op init (cdr seq)))
    )
)

(define (fold-right op initial seq) (accumulate op initial seq))

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest) result
            (iter (op result (car rest)) (cdr rest))
        )
    )
    (iter initial sequence)
)

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

(fold-right + 0 (list 1 2 3))
(fold-left + 0 (list 1 2 3))

;; The op should satisfy commutative criteria.
;; 满足交换律.
;; See for more details: http://community.schemewiki.org/?sicp-ex-2.38
