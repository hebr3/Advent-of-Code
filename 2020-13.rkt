#lang racket
(require threading)
(require racket/match)
(require rackunit)

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
  (~>> "input.txt"
       file->list-of-strings))

(define test
  (~>> "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
"
       test->list-of-strings))


;; Puzzle
(display "test 1: ")
(~>> test)

(display "one: ")
(~>> data)

;(display "test 2: ")
;(~>> test)
;
;(display "two: ")
;(~>> data)

;; Unit Test
