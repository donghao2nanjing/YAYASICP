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

(define (list->tree elements)
    (car (partial-tree elements (length elements)))
)

(define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let
            ((left-size (quotient (- n 1) 2)))
            (let 
                ((left-result (partial-tree elts left-size)))
                (let
                    ((left-tree (car left-result))
                    (non-left-elts (cdr left-result))
                    (right-size (- n (+ left-size 1)))
                    )
                    (let 
                        ((this-entry (car non-left-elts))
                        (right-result (partial-tree (cdr non-left-elts) right-size)))
                        (let 
                            ((right-tree (car right-result))
                            (remaining-elts (cdr right-result)))
                            (cons (make-tree
                                this-entry
                                left-tree
                                right-tree
                            )
                            remaining-elts
                            )
                        )
                    )

                )
            )
        )
    )
)

(define (union-list-set set1 set2)
    (cond 
        ((null? set1) set2)
        ((null? set2) set1)
        (else
            (let 
                (
                    (x1 (car set1))
                    (x2 (car set2))
                )
                (cond 
                    ((> x1 x2) (cons x2 (union-list-set set1 (cdr set2))))
                    ((< x1 x2) (cons x1 (union-list-set set2 (cdr set1))))
                    (else (cons x1 (union-list-set (cdr set2) (cdr set1))))
                )
            )
        )
    )
)

(define (intersection-list-set set1 set2)
    (if (or (null? set1) (null? set2)) '()
        (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2) (cons x1 (intersection-list-set (cdr set1) (cdr set2))))
                ((< x1 x2) (intersection-list-set (cdr set1) set2))
                ((> x1 x2) (intersection-list-set set1 (cdr set2)))
            )
        )
    )
)


(define example-tree-1 (list->tree (list 1 3 5 7 9 11)))
(define example-tree-2 (list->tree (list 5 7 9 11 13 15)))

(display example-tree-1)
(newline)
(display example-tree-2)

(define (union-set set1 set2)
    (list->tree (union-list-set
        (tree->list-2 set1)
        (tree->list-2 set2)
    ))
)

(define (intersection-set set1 set2)
    (list->tree (intersection-list-set
        (tree->list-2 set1)
        (tree->list-2 set2)
    ))
)

(newline)
(display "union-set:\n")
(union-set example-tree-1 example-tree-2)
(newline)
(display "intersection-set:\n")
(intersection-set example-tree-1 example-tree-2)
