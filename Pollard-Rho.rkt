#lang racket

(require "Loops.rkt"
         "Euclidian-Algorithm.rkt")
(provide Pollard)


;;***************************************************************
;; (Pollard n X0) gives a factor of n by using Pollard's rho
;;    algorithm. X0 is the initial term in the rho sequence.
;; Pollard: Nat Nat -> Nat
;; O(m^(3/2)) worst case, where m is the largest divisor of n

(define (Pollard n X0)
  (define (g x) (modulo (add1 (sqr x)) n))
  (gen (list (g X0)
             (g (g X0))
             1)
       (λ (z) (or (not (= 1 (third z)))
                  (= n (third z))))
       (λ (z) (list (g (first z))
                    (g (g (second z)))
                    (gcd (abs (- (first z)
                                 (second z)))
                         n)))
       (λ (z) (if (= n (third z))
                  (Pollard n (add1 X0))
                  (third z)))))


