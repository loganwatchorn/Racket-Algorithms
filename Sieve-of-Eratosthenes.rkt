#lang racket

(require "while.rkt")
(provide Eratosthenes)


;;***************************************************************
;; (sieve lst n) produces a list with all integers in lst which
;;    are not divisible by n
;; sieve: List[Nat...] Nat -> List[Nat...]
;; O(m) where m=(length lst)

(define (sieve lst n)
  (cond
    [(empty? lst)
     empty]
    [(= 0 (modulo (first lst) n))
     (sieve (rest lst) n)]
    [else
     (cons (first lst)
           (sieve (rest lst) n))]))


;;***************************************************************
;; (inv->list inv) prduces a list of prime numbers in ascending
;;    order from the final invariant of (Eratosthenes n)
;; inv->list: List[List[Nat...] List[Nat...]] -> List[Nat...]
;; O(pi(sqrt(n)) where n is the input to Eratosthenes and pi is
;;    the prime-counting function

(define (inv->list inv)
  (while (λ (x) (not (empty? (second x))))
         inv
         (λ (x) (list (cons (first (second x))
                            (first x))
                      (rest (second x))))
         (λ (x) (first x))))


;;***************************************************************
;; (Eratosthenes n) returns a list of all primes between 2 and n
;;   inclusive, where n>2
;; Eratosthenes: Nat -> List[Prime...]
;; O(n*log(log(n)))

(define (Eratosthenes n)
  (while (λ (x) (and (not (empty? (first x)))
                     (<= (expt (first (first x)) 2)
                         n)))                     
         (list (range 3 (add1 n)  2)
               (list 2))
         (λ (x) (list (sieve (first x) (first (first x)))
                      (cons (first (first x))
                            (second x))))
         (λ (x) (inv->list x))))


;;***************************************************************







                     

;;***********************Logan Watchorn**************************
  