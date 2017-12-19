#lang racket

(provide while)

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
