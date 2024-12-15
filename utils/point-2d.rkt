#lang racket/base

(provide point 
         manhattan-distance
         translate-point)

;; Defines a 2D point structure
(struct point [x y] #:transparent)

;; Calculates the Manhattan distance between two points
(define (manhattan-distance p1 p2)
  (if (and (point? p1) (point? p2))
    ;; Absolute difference of x coordinates plus absolute difference of y coordinates
    (+ (abs (- (point-x p1) (point-x p2)))
       (abs (- (point-y p1) (point-y p2))))
    (error "Arguments must be points")))

;; Translate a point by a given point
(define (translate-point pt vec)
  (if (and (point? pt) (point? vec))
      (point (+ (point-x pt) (point-x vec)) (+ (point-y pt) (point-y vec)))
      (error "Arguments must be points")))