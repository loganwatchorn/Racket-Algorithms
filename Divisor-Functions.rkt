#lang racket

(require "Loops.rkt"
         "Prime-Factorization.rkt"
         "Euclidian-Algorithm.rkt")
(provide e
         div-count
         div-sum
         div-product
         totient)

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
;; (div-count n) returns the number of positive divisors of n
;; div-count: Nat -> Nat

(define (div-count n)
  (gen (list (pfact n) 1)
       (λ (x) (empty? (first x)))
       (λ (x) (list (rest (first x))
                    (* (second (first (first x)))
                       (second x))))
       (λ (x) (second x))))

;;***************************************************************
;; (div-sum n) returns the sum of all positive divisors of n
;; div-sum: Nat -> Nat

(define (newterm p e)
  (/ (add1 (expt p (add1 e)))
     (add1 p)))

(define (div-sum n)
  (gen (list (pfact n) 1)
       (λ (x) (not (empty? (first x))))  
       (λ (x) (list (rest (first x))
                    (* (newterm (first (first (first x)))
                                (second (first (first x))))
                       (second x))))
       (λ (x) (second x))))

;;***************************************************************
;; (div-product n) returns the product of all positive divisors of n
;; div-product: Nat -> Nat

(define (div-product n)
  (expt n (/ (div-count n) 2)))


;;***************************************************************
;; (totient n) is the Euler-Totient function. Returns the number
;;    of integers i between 1 and n such that gcd(i,n)=1.
;; (inefficient
;; phi: Nat -> Nat
;; O(n^2)

(define (totient n)
  (gen (list 2 1)
       (λ (x) (= (first x) n))
       (λ (x) (if (= 1 (gcd n (first x)))
                  (list (add1 (first x)) (add1 (second x)))
                  (list (add1 (first x)) (second x))))
       (λ (x) (second x))))
                    

;;***************************************************************
;; (fast-totient pfact) returns the same as (totient n) where
;;    pfact = (pfact n) (note: pfact requires totient)

(define (fast-totient pfact)
  (gen (cons 1 pfact)
       (λ (x) (empty? (rest x)))
       (λ (x) (cons (* (first x)
                       (- (expt (first (second x))
                                (second (second x)))
                          (expt (first (second x))
                                (sub1 (second (second x))))))
                    (rest (rest x))))
       (λ (x) (first x))))


;;***************************************************************








;;***********************Logan Watchorn**************************
