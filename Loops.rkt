#lang racket

(provide gen
         while)


(define (gen invariant done? step finish)
  (if (done? invariant)
      (finish invariant)
      (gen (step invariant)
           done?
           step
           finish)))


(define (while condition
               invariant
               proceed
               terminate)
  (if (condition invariant)
      (while condition
             (proceed invariant)
             proceed
             terminate)
      (terminate invariant)))
