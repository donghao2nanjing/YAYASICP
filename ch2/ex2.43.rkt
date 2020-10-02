#lang sicp 

(define (enumerate-interval start end)
    (if (> start end) nil
        (cons start (enumerate-interval (+ start 1) end))
    )
)

(define (accumulate op init seq)
    (if (null? seq)
        init
        (op (car seq) (accumulate op init (cdr seq)))
    )
)

(define (filter proc l)
    (if (null? l) nil
        (if (proc (car l)) 
            (cons (car l) (filter proc (cdr l)))
            (filter proc (cdr l))
        )
    )
)

(define (flatmap proc seq)
    (accumulate append nil (map proc seq))
)

(define (queens board-size)
    (define empty-board nil)
    ;(define empty-board (list (list nil)))

    (define (adjoin-position newrow col rest)
        (append (list (list newrow col)) rest)
    )

    (define (safe? k positions)
        (define (myand a b) (and a b))

        (define (check-row positions row) 
            (accumulate myand #t (map (lambda (position) (not (= row (car position))))
                positions
            ))
        )

        (define (check-diag positions row col)
            (accumulate myand #t (map (lambda (position) 
                    (not (= (abs (- row (car position))) (abs (- col (cadr position))))))
                positions)
            )
        )

        (if (null? (cdr positions)) #t
            (let ((newposition (car positions)))
                (let ((row (car newposition))
                    (col (cadr newposition))
                    )
                    (and 
                        (check-diag (cdr positions) row col)
                        (check-row (cdr positions) row)
                    )
                )
            )
        )
    )
    (define (queen-cols k) 
        (if (= k 0)
            (list empty-board)
            (filter 
                (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (new-row)
                        (map (lambda (rest-of-queens) 
                            (adjoin-position
                                new-row k rest-of-queens
                            ))
                            (queen-cols (- k 1))
                        )
                    )
                    (enumerate-interval 1 board-size)
                )
            )
        )
    )
    (queen-cols board-size)
)

(define (display-queens n)
    (display n)
    (display " queens")
    (newline)
    (display (queens n))
    (newline)
)

(display-queens 1)
(display-queens 2)
(display-queens 3)
(display-queens 4)
(display-queens 5)
(display-queens 6)
(display-queens 7)
(display-queens 8)

;;; Remark: 
; Because to get the result of (queen 8), this program should run (queen 7) for 8 times!
; To get the result of (queen 7), this program should run (queen 6) for 8 times!
; To get the result of (queen 6), this program should run (queen 5) for 8 times!
; That's why it runs so slow!