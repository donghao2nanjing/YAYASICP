#lang sicp 

(define (square-tree t)
    (map (lambda (subtree) 
        (if (pair? subtree) (square-tree subtree)
            (* subtree subtree)
        )
    ) t)
)

(square-tree
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7))
)
