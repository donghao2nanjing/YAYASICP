#lang racket

; See YASICP for reference.
(define table (make-hash))
(define (put op type item)
    (hash-set! table (cons op type) item)
)
(define (get op type)
    (hash-ref table (cons op type) #f)
)

; a. Explain: use data directed representation to reimplement deriv for sum and product.
; Why? 

(define (variable? e) (symbol? e))
(define (=number? e n) (if (number? e) (= n e) #f))
(define (same-variable? v1 v2) 
    (and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
    (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp) var))
    )
)

(define (addend e) (car e))
(define (augend e) (cadr e))
(define (make-sum a1 a2) 
    (cond 
        ((=number? a1 0) a2 )
        ((=number? a2 0) a1 )
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2)))
)

(define (multiplier e) (car e))
(define (multiplicand e) (cadr e))
(define (make-product m1 m2) 
    (cond 
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))
    )
)

(define (make-exponentiation base exp) 
    (cond
        ((=number? base 1) 1)
        ((=number? exp 0) 1)
        (else (list '** base exp))    
    )
)
(define (pow-base e) (car e))
(define (pow-exp e) (cadr e))

(define (install-deriv-sum-package)
    (define (deriv-sum exp var)
        (make-sum (deriv (addend exp) var) (deriv (augend exp) var))
    )
    (put '+ 'deriv deriv-sum)
    'done
)

(define (install-deriv-product-package)
    (define (deriv-prod exp var)
        (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
            (make-product (multiplicand exp) (deriv (multiplier exp) var))
        )
    )
    (put '* 'deriv deriv-prod)
    'done
)

(define (install-deriv-exponent-package)
    (define (deriv-exp exp var)
        (make-product 
            (pow-exp exp)
            (make-product
                (make-exponentiation (pow-base exp) (- (pow-exp exp) 1))
                (deriv (pow-base exp) var)
            )
        )
    )
    (put '** 'deriv deriv-exp)
    'done
)

(install-deriv-sum-package)
(install-deriv-product-package)
(install-deriv-exponent-package)

(define ex-exp '(+ x (+ (** x 3) (* 3 y))))
(deriv ex-exp 'x)
