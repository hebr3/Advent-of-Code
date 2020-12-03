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
  (~> "input.txt"
      open-input-file
      (read-line _ 'return-linefeed)))

(define (tree? str count)
  (let ([L (string->list str)])
    (char=? #\# (list-ref L (modulo count (length L)))))) 

(define (find-trees list-of-strings val)
  (for/list ([s list-of-strings]
             [i (length list-of-strings)])
    (and #t (tree? s (* i val)))))

(define (find-trees2 list-of-strings val)
  (for/list ([s list-of-strings]
             [i (length list-of-strings)])
    (and (even? i) (tree? s (* i (/ val 2))))))

(display "test 1: ")
(* (~> test
       (string-split _ "\n")
       (find-trees _ 1)
       (count identity _))
   (~> test
       (string-split _ "\n")
       (find-trees _ 3)
       (count identity _))

   (~> test
       (string-split _ "\n")
       (find-trees _ 5)
       (count identity _))

   (~> test
       (string-split _ "\n")
       (find-trees _ 7)
       (count identity _))

   (~> test
       (string-split _ "\n")
       (find-trees2 _ 1)
       (count identity _)))

(display "one: ")
(* (~> data
       (string-split _ "\n")
       (find-trees _ 1)
       (count identity _))
   (~> data
       (string-split _ "\n")
       (find-trees _ 3)
       (count identity _))

   (~> data
       (string-split _ "\n")
       (find-trees _ 5)
       (count identity _))

   (~> data
       (string-split _ "\n")
       (find-trees _ 7)
       (count identity _))

   (~> data
       (string-split _ "\n")
       (find-trees2 _ 1)
       (count identity _)))
;(display "test 2: ")
;(~> test
;    (string-split _ "\n")
;    find-trees-for-slopes)

;(display "two: ")
;(~> data)