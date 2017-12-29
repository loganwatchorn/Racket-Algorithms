#lang racket

(require "Loops.rkt")

(define (g x n)
  (modulo (+ 1 (sqr x))
          n))

;;***************************************************************
;; (index g X0) finds the value in the rho cycle where the two
;;     pointers (one moving two indices at a time - the hare, and
;;     the other moving one index at a time - the tortoise) meet.
;; index: Proc Nat -> Nat

(define (Tortoise-vs-Hare g X0 n)
  (gen (list (g X0 n) (g (g X0 n)))
       (λ (z) (= (first z) (second z)))
       (λ (z) (list (g (first z) n)
                    (g (g (second z) n) n)))
       (λ (z) (first z))))

;;***************************************************************
;; (mu g X0) finds the index in the rho cycle where the cycle
;;    begins
;; mu: Proc Nat -> Nat

(define (mu g X0 n)
  (gen (list 0
             X0
             (Tortoise-vs-Hare g X0 n))
       (λ (y) (= (second y) (third y)))
       (λ (y) (list (add1 (first y))
                    (g (second y) n)
                    (g (third y) n)))
       (λ (y) (list (g (second y) n)
                    (second y)
                    (first y)))))

;;***************************************************************
;; (Floyd g X0) uses Floyd's cycle-detection algorithm to
;;    determine the period and start index of a cyclic sequence
;; Floyd: Proc Nat -> List[Nat Nat]

(define (Floyd g X0 n)
  (gen (cons 1
             (mu g X0 n))
       (λ (x) (= (second x) (third x)))
       (λ (x) (list (add1 (first x))
                    (second x)
                    (g (third x) n)))
       (λ (x) (list (first x)
                    (fourth x)))))
  
  