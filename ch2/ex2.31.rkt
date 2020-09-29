#lang sicp 

(define square (lambda (x) (* x x)) )

(define (tree-map tree term)
    (map (lambda (subtree) 
        (if (pair? subtree) (tree-map subtree term)
            (term subtree)
        )
    ) tree)
)

(tree-map
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7))
    square
)
