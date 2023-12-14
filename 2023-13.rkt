#lang racket
(require racket/match)
(require "util.rkt")

(define data (input->data "input/2023-13.txt"))

(define test "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

;;

(define (transpose los)
  (for/list ([x (string-length (first los))])
    (apply string (reverse (for/list ([s los]) (string-ref s x))))))

(define (horizontal-reflection? los)
  (define (iter i)
    ;;(println 'i)
    (cond
      [(= i (length los)) #f]
      [else
       (let ([upper-rows (reverse (take los i))]
             [lower-rows (drop los i)])
         (cond
           [(for/and ([u upper-rows][l lower-rows])
              ;;        (println (list i u l))
              (string=? u l))
            i]
           [else
            (iter (add1 i))]))]))
  (iter 1))

(define (vertical-reflection? los)
  (horizontal-reflection? (transpose los)))

(define (part-A L)
  (let ([blocks (parse-blocks L)])
    (for/sum ([block blocks])
      (let ([hori (horizontal-reflection? block)]
            [vert (vertical-reflection? block)])
        (when (not (or vert hori))
          (for ([line block]) (displayln line))
          (displayln ""))
        (or vert
            (* 100 (or hori 0)))))))
        

(part-A test)
(part-A data)

;;

(define (part-B L)
  L)
              

;; (part-B test)
;; (part-B data)

