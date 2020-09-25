#lang sicp 

(define (square n) (* n n))
(define (even? n) (= (remainder n 2) 0))

(define (fast_prime? n times)
    (cond 
        ((= times 0) true)
        ((fermat_test n) (fast_prime? n (- times 1)))
        (else false)
    )
)

(define (fermat_test n) 
    (define (try_it a)
        (= (expmod a n n) a)
    )
    (if (> n 4294967087) (try_it (+ 1 (random 4294967087)))
    (try_it (+ 1 (random (- n 1)))))
)

(define (fast_exp b n)
    (cond ((= n 0) 1)
        ((even? n) (square (fast_exp b (/ n 2))))
        (else (* b (fast_exp b (- n 1))))
    )
)

;;; I think the problem lies in the calculation of remainder!
;;; This method involves calculation of big numbers!
(define (expmod base exp m)
    (remainder (fast_exp base exp) m)
)

(define (prime? n) (fast_prime? n 10))

(define (timed_prime_test n)
    (start_prime_test n (runtime))
)

(define (start_prime_test n start_time)
    (define (report_prime_info n start_time)
        (report_prime (- (runtime) start_time))
        (display n)
        (newline) 
    )
    (if (prime? n)
        (report_prime_info n start_time)
        ; (report_not_prime (- (runtime) start_time))
    )
)

(define (report_prime elapsed_time)
    (display "It is prime, elapsed time is:")
    (display elapsed_time)
    (display " number: ")
)

(define (report_not_prime elapsed_time)
    (display "It is not prime, elapsed time is:")
    (display elapsed_time)
    (display " number: ")
)

(define (next_start n) 
    (if (= (remainder n 2) 0) (+ n 1)
        (+ n 2)
    )
)

(define (search_for_primes start end count) 
    (if (> count 0) (timed_prime_test start) )
    (if (and (< (next_start start) end) (> count 0) )
        (if (prime? start) 
            (search_for_primes (next_start start) end (- count 1))
            (search_for_primes (next_start start) end count)
        )
    )
)

(search_for_primes 1000000 10000000 3)
(newline)

;;; Results:  Much slower!

(search_for_primes 100000000000 1000000000000 3)
(newline)

(search_for_primes 1000000000000 10000000000000 3)
(newline)

(search_for_primes 10000000000000 100000000000000 3)
(newline)

;;; Results: Much slower!