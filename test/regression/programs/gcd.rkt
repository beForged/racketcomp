#lang racket 

(define (gcd a b)
    (cond
        [(> a b) (gcd b (- a b))]
        [(< a b) (gcd b (- b a))]
        [else a]))

(gcd 15 3)
