#lang racket

(require "Loops.rkt"
         "Prime-Factorization.rkt")
(provide e
         tau
         sigma
         rho)

;;***************************************************************
;; (e p n) returns the exponent the prime p is raised to in the
;;    prime factorisation of n.
;; e: Nat Nat -> Nat

(define (e p n)
  (gen (pfact n)
       (λ (x) (or (= p (first (first x))))
                  (empty? x))
       (λ (x) (rest x))
       (λ (x) (if (empty? x)
                  0
                  (second (first x))))))


;;***************************************************************
;; (tau n) returns the number of positive divisors of n
;; tau: Nat -> Nat

(define (tau n)
  (gen (list (pfact n) 1)
       (λ (x) (empty? (first x)))
       (λ (x) (list (rest (first x))
                    (* (second (first (first x)))
                       (second x))))
       (λ (x) (second x))))

;;***************************************************************
;; (sigma n) returns the sum of all positive divisors of n
;; sigma: Nat -> Nat

(define (newterm p e)
  (/ (add1 (expt p (add1 e)))
     (add1 p)))

(define (sigma n)
  (gen (list (pfact n) 1)
       (λ (x) (not (empty? (first x))))  
       (λ (x) (list (rest (first x))
                    (* (newterm (first (first (first x)))
                                (second (first (first x))))
                       (second x))))
       (λ (x) (second x))))

;;***************************************************************
;; (rho n) returns the product of all positive divisors of n
;; rho: Nat -> Nat

(define (rho n)
  (expt n (/ (tau n) 2)))


;;***************************************************************








;;***********************Logan Watchorn**************************