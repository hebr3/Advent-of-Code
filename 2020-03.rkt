#lang racket
(require threading)
(require racket/match)

(define test
  "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(define data
  (~> "input/2020-03w.txt"
      open-input-file
      (read-line _ 'return-linefeed)))

(define (tree? str offset)
  (~> (string-ref str (modulo offset (string-length str)))
      (char=? #\#)))

(define (find-trees list-of-strings slope)
  (for/sum ([s list-of-strings]
            [i (length list-of-strings)])
    (if (tree? s (* i slope)) 1 0)))

(define (find-trees2 list-of-strings val)
  (for/sum ([s list-of-strings]
            [i (length list-of-strings)])
    (if (and (even? i)
             (tree? s (* i val)))
        1 0)))


(display "test 1: ")
(~> test
    (string-split _ "\n")
    (find-trees _ 3))

(display "one: ")
(~> data
    (string-split _ "\n")
    (find-trees _ 3))

(display "test 2: ")
(* (~> test
       (string-split _ "\n")
       (find-trees _ 1))
   (~> test
       (string-split _ "\n")
       (find-trees _ 3))

   (~> test
       (string-split _ "\n")
       (find-trees _ 5))

   (~> test
       (string-split _ "\n")
       (find-trees _ 7))

   (~> test
       (string-split _ "\n")
       (find-trees2 _ 1/2)))

(display "two: ")
(* (~> data
       (string-split _ "\n")
       (find-trees _ 1))
   (~> data
       (string-split _ "\n")
       (find-trees _ 3))

   (~> data
       (string-split _ "\n")
       (find-trees _ 5))

   (~> data
       (string-split _ "\n")
       (find-trees _ 7))

   (~> data
       (string-split _ "\n")
       (find-trees2 _ 1/2)))
