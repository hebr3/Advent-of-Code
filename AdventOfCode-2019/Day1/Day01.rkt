#lang racket

(define input
  (file->lines "input.txt"))

(define (helper num)
  (let ([fuel (- (floor (/ num 3)) 2)])
    (if (< fuel 0)
        0
        fuel)))

(helper 12)
(helper 14)
(helper 1969)
(helper 100756)

(apply +
       (map (λ (x) (helper (string->number x))) input))

(define (helper2 num)
  (define (iter extra)
    (let ([next (helper extra)])
      (if (zero? next)
          extra
          (+ extra (iter next)))))
  (iter (helper num)))

(helper2 14)
(helper2 1969)
(helper2 100756)

(apply +
       (map (λ (x) (helper2 (string->number x))) input))
