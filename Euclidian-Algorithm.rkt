#lang racket

(require "Loops.rkt")
(provide gcd
         dio)

;;***************************************************************
;; (gcd a b) uses the Euclidian algorithm to find the greatest
;;     common divisor of integers a and b
;; gcd: Nat Nat -> Nat

(define (gcd a b)
  (gen (if (<= a b)
           (list a b)
           (list b a))
       (λ (x) (= 0 (second x)))
       (λ (x) (list (second x)
                    (remainder (first x)
                               (second x))))
       (λ (x) (first x))))


;;***************************************************************
;; (dio a b) uses the extended Euclidian algorithm to find the
;;     general solution to the linear diophantine equation
;;     a*s + b*t = gcd(a,b)

(define-struct step (a b q r))

(define (dio a b)
  (gen
   (if (<= a b)
       (cons (make-step a b (quotient a b) (remainder a b)) '())
       (cons (make-step b a (quotient b a) (remainder b a)) '()))
   (λ (x) (= 0 (step-r (first x))))
   (λ (x) (cons (make-step (step-b (first x))
                           (step-r (first x))
                           (quotient (step-b (first x))
                                     (step-r (first x)))
                           (remainder (step-b (first x))
                                      (step-r (first x))))
                x))
   (λ (x) (if (empty? (rest x))
              1
              (back-sub (- (step-q (second x)))
                        1
                        (cddr x)
                        (step-b (first x)))))))

       
(define (back-sub s t rows gcd)
  (if (empty? (rest rows))
      (end-statement
       (number->string (step-a (first rows)))
       (number->string (step-b (first rows)))
       (number->string s)
       (number->string t)
       (number->string gcd)
       (number->string (/ (- (step-b (first rows))) gcd))
       (number->string (/ (step-a (first rows)) gcd)))
      (back-sub (- t (* s (step-q (first rows))))
                s
                (rest rows)
                gcd)))


(define (end-statement a b s t gcd i j)
  (string-append
   "For a=" a " and b=" b " and a*s+b*t=gcd(a,b)=" gcd
   ", the general solution for (s,t) is (s,t)=("
   s "," t ")+k*(" i "," j ") for all k∈Z"))


;;***************************************************************








;;***********************Logan Watchorn**************************