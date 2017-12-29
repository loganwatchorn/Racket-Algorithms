#lang racket

(require "Loops.rkt"
         "Euclidian-Algorithm.rkt"
         "Modular-Arithmetic.rkt"
         "Divisor-Functions.rkt")

(provide pseudoprime?)



;;***************************************************************
;; (find-a n) finds an integer a such that 1<a<n and gcd(a,n)=1
;; find-a: Nat -> Nat
;; O(n)

(define (find-a int)
  (gen int
       (λ (a) (= 1 (gcd a int)))
       (λ (a) (add1 a))
       (λ (a) a)))


;;***************************************************************
;; (find-d n) finds odd integer d such that (2^s)*d = n for some
;;    integer s
;; O(log(n))


(define (find-d n)
  (gen (list (sub1 n) 0)
       (λ (x) (not (= 0 (modulo (first x) 2))))
       (λ (x) (list (/ (first x) 2) (add1 (second x))))
       (λ (x) (first x))))


;;***************************************************************
;; (pseudoprime? n) determines if an odd integer n is a
;;    Miller-Rabin pseudoprime. 1/256 accuracy
;; pseudoprime?: Nat -> Bool
;; O(n^2)

(define (pseudoprime-help n d a)
  
  (define phi (totient n))
  (define phi2 (totient phi))
  (define d_mod_phi (modulo d phi))
  
  (if (= 1 (modulo (expt a d) n))
      true
      (gen 0
           (λ (r) (or (= r n)
                      (= (sub1 n)
                         (mod-expt
                          (mod-expt a
                                    (mod-expt 2
                                              (modulo r phi2)
                                    phi)
                                    n)
                          d_mod_phi
                          n))))
           (λ (r) (add1 r))
           (λ (r) (= r n)))))


(define (pseudoprime? n)
  (define d (find-d n))
  (and (pseudoprime-help n d 2)
       (pseudoprime-help n d (sub1 n))
       (pseudoprime-help n d (find-a (floor (* 1/3 n))))
       (pseudoprime-help n d (find-a (floor (* 2/3 n))))))


;;***************************************************************













;;***********************Logan Watchorn**************************