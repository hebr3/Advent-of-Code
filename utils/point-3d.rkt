#lang racket/base

(provide point
         manhattan-distance)

(struct point [x y z] #:transparent)

(define (manhattan-distance p1 p2)
  (+ (abs (- (point-x p1) (point-x p2)))
     (abs (- (point-y p1) (point-y p2)))
     (abs (- (point-z p1) (point-z p2)))))