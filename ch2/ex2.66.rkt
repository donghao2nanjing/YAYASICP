#lang sicp 

(define (lookup given-key set-of-records)
    (cond ((null? set-of-records) false)
        ((= given-key (key (car set-of-records))) (car set-of-records))
        ((> given-key (key (car set-of-records))) (right-branch set-of-records))
        ((< given-key (key (car set-of-records))) (left-branch set-of-records))
    )
)

