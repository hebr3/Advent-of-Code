#lang racket
(require threading)
(require racket/match)
(require rackunit)

(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)))

;; Structs

;; Functions
(define (id x) x)
  
;; Data
(define data
  (~>> "input.txt"
       (file->list-of-strings)
       (string-split _ "\n")))

;; Test
(define test
  (~> ""
      (string-split _ "\n")))

(define test2
  (~> ""
      (string-split _ "\n")))

;; Puzzle

(display "test 1: ")
(~>> test)

(display "one: ")
(~>> data)

(display "test2: ")
(~>> test2)

(display "two: ")
(~>> data)

