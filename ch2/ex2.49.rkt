#lang sicp 

(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2)
)

(define (origin-frame frame)
    (car frame)
)

(define (edge1-frame frame)
    (cadr frame)
)

(define (edge2-frame frame)
    (cadr (cdr frame))
)

(define (frame-coord-map frame)
    (lambda (v)
        (add-vect 
            (origin-frame frame)
            (add-vect
                (scale-vect (xcor-vect v) (edge1-frame frame))
                (scale-vect (ycor-vect v) (edge2-frame frame))
            )
        )
    
    )
)

(define (make-vect x y)
    (list x y)
)

(define (xcor-vect v)
    (car v)
)

(define (ycor-vect v)
    (cadr v)
)

(define (add-vect v1 v2)
    (make-vect
        (+ (xcor-vect v1) (xcor-vect v2))
        (+ (ycor-vect v1) (ycor-vect v2))
    )
)

(define (sub-vect v1 v2)
    (make-vect
        (- (xcor-vect v1) (xcor-vect v2))
        (- (ycor-vect v1) (ycor-vect v2))
    )
)

(define (scale-vect v s)
    (make-vect
        (* s (xcor-vect v))
        (* s (ycor-vect v))
    )
)

(define (make-segment v1 v2)
    (list v1 v2)
)

(define (start-segment s)
    (car s)
)

(define (end-segment s)
    (cadr s)
)

(define (segments->painter segment-list)
    (lambda (frame)
        (for-each 
            (lambda (segment)
                (draw-line
                ((frame-coord-map frame)
                    (start-segment segment)
                )
                ((frame-coord-map frame)
                    (end-segment segment)
                )
            )
        )    
        segment-list
    )
)

(define (painter-a frame)
    ((segments->painter
        (list 
            (make-segment (make-vect 0 0) (make-vect 0 1))
            (make-segment (make-vect 0 0) (make-vect 1 0))
            (make-segment (make-vect 1 1) (make-vect 0 1))
            (make-segment (make-vect 1 1) (make-vect 1 0))
        )
    ) frame)
)

(define (painter-b frame)
    ((segments->painter
        (list 
            (make-segment (make-vect 0 0) (make-vect 1 1))
            (make-segment (make-vect 0 1) (make-vect 1 0))
        )
    ) frame)
)

(define (painter-c frame)
    ((segments->painter
        (list 
            (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
            (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
            (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
            (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
        )
    ) frame)
)

; wave? I don't want to do that.
