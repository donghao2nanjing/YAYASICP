#lang sicp 

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
    (list left
        right 
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))
    )
)

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree) (if (leaf? tree) (list (symbol-leaf tree)) (caddr tree)))
(define (weight tree)
    (if (leaf? tree) (weight-leaf tree) (cadddr tree))
)

(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits) '()
            (let ((next-branch (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)
                )
            )
        )
    )
    (decode-1 bits tree)
)

(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH:" bit))
    )
)

(define (adjoin-set x set) 
    (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))
    )
)

(define (make-leaf-set pairs)
    (if (null? pairs) 
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair)
                (cadr pair) 
            ) (make-leaf-set (cdr pairs)))
        )
    )
)

(define sample-tree
    (make-code-tree (make-leaf 'A 4)
        (make-code-tree (make-leaf 'B 2)
            (make-code-tree (make-leaf 'D 1)
                (make-leaf 'C 1)
            )
        )
    )
)

(define (encode message tree) 
    (if (null? message)
        '()
        (append (encode-symbols (car message) tree) (encode (cdr message) tree))
    )
)

(define (encode-symbols original-message tree)
    (define (in-symbols char symbols-list)
        (if (null? symbols-list) #f
            (or (eq? char (car symbols-list)) (in-symbols char (cdr symbols-list)))
        )
    )
    (define (encode-1 symbol tree)
        (if (leaf? tree) '()
            (cond ((in-symbols symbol (symbols (left-branch tree)))
                    (cons 0 (encode-1 symbol (left-branch tree)))) 
                ((in-symbols symbol (symbols (right-branch tree)))
                    (cons 1 (encode-1 symbol (right-branch tree))))
                (else (error "error ENCODE:" symbol))
            )
        )
    )
    (if (null? original-message) '()
        (append (encode-1 (car original-message) tree) 
            (encode-symbols (cdr original-message) tree))
    )
)

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs))
)

(define (successive-merge leaf-set)
    (if (null? (cdr leaf-set)) (car leaf-set)
        (let ((current-set (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set))))
            (successive-merge current-set)
        )
    )
)

(define n5-symbols (list 
    (list 'A 1)
    (list 'B 2)
    (list 'C 4)
    (list 'D 8)
    (list 'E 16)
    )
)

(define n5-tree (generate-huffman-tree n5-symbols))

(display n5-tree)
(newline)

(define n10-symbols (list 
    (list 'A 1)
    (list 'B 2)
    (list 'C 4)
    (list 'D 8)
    (list 'E 16)
    (list 'F 32)
    (list 'G 64)
    (list 'H 128)
    (list 'I 256)
    (list 'J 512)
    )
)

(define n10-tree (generate-huffman-tree n10-symbols))

(display n10-tree)

;; For n = 5, 1 bit is neeeded for most frequent symbol, 4 bits are needed for least frequent symbol.
;; For n = 10, 1 bit is neeeded for most frequent symbol, 9 bits are needed for least frequent symbol.
;; For n, 1 bit is neeeded for most frequent symbol, n-1 bits are needed for least frequent symbol.
