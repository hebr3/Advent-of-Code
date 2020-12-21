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
  (~>> "input/2020-21.txt"
       file->list-of-strings
       (map string->0or1)))

(define test
  (~>> ""
       test->list-of-strings
       (map string->0or1)))


;; Puzzle
(display "test 1: ")
(~>> test)

;(display "one: ")
;(~>> data)

;(display "test 2: ")
;(~>> test)
;
;(display "two: ")
;(~>> data)

;; Unit Test
