#lang racket
(require "utils/matrix.rkt")
;(require "utils/point-2d.rkt")
(require "utils/input.rkt")

(define data (input->data "input/2023-24.txt"))

(define test "19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3")

;;

(struct hailstone [x dx y dy] #:transparent)

(define (string->point str)
  (let* ([parts (string-split str #px"(, )|( @ )")]
         [numbs (map (compose string->number string-trim) parts)])
    (match-let ([(list x y _ dx dy _) numbs])
      (hailstone x dx y dy))))

(define (collision? p1 p2 x y)
  (match-let ([(hailstone x1 dx1 y1 dy1) p1]
              [(hailstone x2 dx2 y2 dy2) p2])
    (and ;(<= 7 x 27)
         ;(<= 7 y 27)
         (<= 200000000000000 x 400000000000000)
         (<= 200000000000000 y 400000000000000)
         (<= 0 (/ (- x x1) dx1))
         (<= 0 (/ (- y y1) dy1))
         (<= 0 (/ (- x x2) dx2))
         (<= 0 (/ (- y y2) dy2)))))

(define (check-intersection p1 p2)
  (match-let ([(hailstone x1 dx1 y1 dy1) p1]
              [(hailstone x2 dx2 y2 dy2) p2])
    (let ([m1 (/ dy1 dx1)]
          [m2 (/ dy2 dx2)])
      (cond
        [(= m1 m2)
         #f]
        [else
         (let* ([m1x1 (* m1 x1)]
                [m2x2 (* m2 x2)]
                [X (/ (+ m1x1 (- m2x2) y2 (- y1)) (- m1 m2))]
                [Y (+ (* m1 (- X x1)) y1)])
           (collision? p1 p2 X Y))]))))

(define (part-A L)
  (let* ([rows (string-split L "\n")]
         [hailstones (map string->point rows)])
    (for/sum ([cb (combinations hailstones 2)])
      (match-let ([(list p1 p2) cb])
        (if (check-intersection p1 p2) 1 0)))))

(part-A test)
(part-A data)

;;

(define (part-B L)
  L)

;(part-B test)
;(part-B data)

