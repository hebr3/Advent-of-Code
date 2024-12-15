#lang racket
(require "utils/matrix.rkt")
(require "utils/point-2d.rkt")
(require "utils/input.rkt")

(define data (input->data "input/2019-10.txt"))

(define test0 ".#..#
.....
#####
....#
...##")

(define test1 "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")

(define test2 "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.
")

(define test3 ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..
")

(define test4 ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##
")

;;

(define (norm p1 p2)
  (match-let ([(point r1 c1) p1]
              [(point r2 c2) p2])
    (cond
      [(= c1 c2) (if (< r1 r2) +inf.0 -inf.0)]
      [(= r1 r2) (if (< c1 c2) 1/100 -1/100)]
      [else      (/ (- r1 r2) (- c1 c2))])))

(define (part-A L)
  (let* ([matrix (string->matrix L)]
         [R (length matrix)]
         [C (length (first matrix))]
         [G (make-hash)])
    (for* ([r R][c C])
      (when (char=? #\# (matrix-ref matrix r c))
        (hash-set! G (point r c) '())))
    (let ([points (map car (hash->list G))])
      (for ([p1 points])
        (for ([p2 points] #:when (not (equal? p1 p2)))
          (displayln (list p1 p2 (norm p1 p2)))
          (hash-set! G p1 (remove-duplicates (cons (norm p1 p2) (hash-ref G p1)))))))
    (for ([r R])
      (for ([c C])
        (if (hash-has-key? G (point r c))
            (display (length (hash-ref G (point r c))))
            (display #\.)))
      (displayln ""))))


(part-A test0)
;(part-A data)

;;

(define (part-B L)
  L)

;(part-B test)
;(part-B data)

