#lang racket

(require "while.rkt"
         "Sieve-of-Eratosthenes.rkt")
(provide pfact)

;;***************************************************************
;; (prime? n) returns true if n is true, false otherwise.
;;    Determines the primality of n by checking for divisors up
;;    to (sqrt n)
;; prime?: Nat -> Bool
;; O(sqrt(n))

(define (prime? n)
  (if (= n 2)
      true
      (while (λ (x) (and (<= x (sqrt n))
                         (not (= 0 (modulo n x)))))
             2
             add1
             (λ (x) (not (= 0 (modulo n x)))))))


;;***************************************************************
;; (pfact n) returns the prime factorisation of n in the form of
;;  list of pairs, where the first of each pair is a prime and
;;  the second of the pair is the prime's corresponding exponent.
;; pfact: Nat -> Listof[Listof[Nat Nat]...]
;; O(g(x)) where (primes n)∈ O(g(x))

(define (primes n) (Eratosthenes n))

(define (pfact n)
  (if
   (prime? n)
   "prime"
   (while (λ (x) (and (not (empty? (first x)))
                      (not (= 1 (second x)))))
          (list (primes (floor (/ n 2)))
                n
                '())
          (λ (x)
            (cond
              [(not (= 0 (modulo (second x)
                                 (first (first x)))))
               (list (rest (first x))
                     (second x)
                     (third x))]
              [(empty? (third x))
               (list (first x)
                     (/ (second x)
                        (first (first x)))
                     (list (list (first (first x))
                                 1)))]
              [(= (first (first x))
                  (first (first (third x))))
               (list (first x)
                     (/ (second x)
                        (first (first x)))
                     (cons (list (first (first (third x)))
                                 (add1
                                  (second (first (third x)))))
                           (rest (third x))))]
              [else
               (list (first x)
                     (/ (second x)
                        (first (first x)))
                     (cons (list (first (first x))
                                 1)
                           (third x)))]))
          (λ (x) (third x)))))


;;***************************************************************                   
                
                
                










;;***********************Logan Watchorn**************************
  