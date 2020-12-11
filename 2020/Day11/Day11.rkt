#lang racket
(require threading)
(require racket/match)

;; Prep
(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)
      (string-split "\n")))

(define (test->list-of-strings str)
  (string-split str "\n"))

;; Structs

;; Functions
(define (id x) x)

;; Data
(define test
  (test->list-of-strings ""))

;(define data (file->list-of-strings "input"))

;; Puzzle
(display "test 1: ")
(~>> test)

;(display "one: ")
;(~>> data)
;
;(display "test 2: ")
;(~>> test)
;
;(display "two: ")
;(~>> data)
