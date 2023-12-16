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

(define (calc-diff-of-strings s1 s2)
  (for/sum ([c1 s1][c2 s2]) (if (not (char=? c1 c2)) 1 0)))

(define (find-horizontal-smudge los)
  (for/list ([i (range 1 (length los))])
    (let ([upper (reverse (take los i))]
          [lower (drop los i)])
      (for/sum ([u upper][l lower])
        (calc-diff-of-strings u l)))))

(define (find-vertical-smudge los)
  (find-horizontal-smudge (transpose los)))

(define (find-smudges blocks)
  (for/list ([block blocks])
      (list (index-of (find-horizontal-smudge block) 1)
            (index-of (find-vertical-smudge block) 1))))

(define (part-B L)
  (let* ([blocks (parse-blocks L)]
         [smudges (find-smudges blocks)])
    (for/sum ([smudge smudges])
      (match-let ([(list h v) smudge])
        (if h
            (* 100 (add1 h))
            (add1 v))))))
              

(part-B test)
(part-B data)

