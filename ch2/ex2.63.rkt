#lang sicp 


(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
    (list entry left right))

(define (element-of-set? x set)
    (cond ((null? set) #f)
        ((= x (entry set)) #t) 
        ((< x (entry set)) 
            (element-of-set? x (left-branch set)))
        ((> x (entry set))
            (element-of-set? x (right-branch set)))
    )
)

(define (adjoin-set x set)
    (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) 
            (make-tree (entry set)
                (adjoin-set x (left-branch set))
                (right-branch set)
            )
        )
        ((> x (entry set))
            (make-tree (entry set)
                (left-branch set)
                (adjoin-set x (right-branch set))
            )
        )
    )
)

(define (tree->list-1 tree)
    (if (null? tree) '()
        (append (tree->list-1 (left-branch tree))
            (cons (entry tree) (tree->list-1 (right-branch tree)))
        )
    )
)

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree) result-list
            (copy-to-list (left-branch tree) (cons (entry tree) (copy-to-list 
                (right-branch tree) result-list )))
        )
    )
    (copy-to-list tree '())
)

(define l (list 1 2 3 4 5 6 7))

(define (make-example-tree example-list)
    (if (null? example-list) '()
        (make-tree (car example-list)
            '()
            (make-example-tree (cdr example-list))
        )
    )
)

(define example-tree (make-example-tree l))
(display example-tree)

(newline)
(tree->list-1 example-tree)
(newline)
(tree->list-2 example-tree)

; a: the 2 methods produce the same list
; b: the difference of the growth of time of the 2 methods lies in the procedure append.
; tree->list-1 runs recursively. tree->list-2 runs iteratively. 
; cons consumes less time than append, tree->list-2's growth of run steps grows more slowly. 
