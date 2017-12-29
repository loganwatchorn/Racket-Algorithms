#lang racket

(require "Loops.rkt"
         "Divisor-Functions.rkt")
(provide mod-expt)


;;***************************************************************
;; (mod-expt a b n) computes (a^b)mod(n)
;; mod-expt: Int Nat Nat -> Nat
;; O(b)

(define (mod-expt a b n)
  (gen (list 1 b)
       (λ (x) (= 0 (second x)))
       (λ (x) (if (= 0 (modulo (second x) 2))
                  (list (modulo (* a (first x)) n)
                        (sub1 (second x)))
                  (list)
       (λ (x) (first x))))


;;***************************************************************















