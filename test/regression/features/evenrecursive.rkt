#lang racket
(begin (define (even? x) (if (zero? x) #t (odd? (- x 1)))) (define (odd? x) (if (zero? x) #f (even? (- x 1)))) (even? 101))
