#lang racket
(require threading)
(require racket/match)
(require rackunit)
(require racket/set)

;; Prep
(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)
      (string-split "\n")))

(define (test->list-of-strings str)
  (~> str
      (string-split "\n")))

;; Struct

;; Functions
(define (id x) x)

;; Data
(define data
  (~>> "input/2020-18.txt"
       file->list-of-strings))

(define test
  (~>> "1 + 2 * 3 + 4 * 5 + 6
1 + (2 * 3) + (4 * (5 + 6))
2 * 3 + (4 * 5)
5 + (8 * 3 + 9 + 3 * 4 * 3)
5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))
((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2
"
       test->list-of-strings
       ))


;; Puzzle
(display "test 1: ")
(~>> test)

(display "one: ")
;(~>> data)

;(display "test 2: ")
;(~>> test)
;
;(display "two: ")
;(~>> data)

;; Unit Test
