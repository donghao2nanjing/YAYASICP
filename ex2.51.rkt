#lang sicp 

(define (below painter1 painter2)
    (let ((split-point (make-vect 0.0 0.5)))
        (let ((paint-lower
            (transform-painter painter1
                (make-vect 0.0 0.0)
                (make-vect 1.0 0.0)
                split-point
            )
        )(paint-upper
            (transform-painter painter2
                split-point
                (make-vect 1.0 0.0)
                (make-vect 0.5 1.0)
            )
        )
        )
        (lambda (frame) 
            (paint-upper frame)
            (paint-lower frame)
        )
        )
    )
)

; It's boring to me :( .
; So there is no the other version.
