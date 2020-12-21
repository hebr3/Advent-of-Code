#lang racket
(require threading)
(require racket/match)
(require rackunit)

(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)))

;; Functions
(define (count-votes groups-votes combine-function)
  (~>> groups-votes
      (string-split _ "\n")
      (map string->list)
      (apply append)
      (apply set)
      set-count))

(define (count-any-yes groups-votes)
  (count-votes groups-votes *))

;; Test
(define test
  "abc

a
b
c

ab
ac

a
a
a
a

b")

(check-eq? (identity 1)
           1)
(check-eq? (identity 1)
           1)
(check-eq? (identity 1)
           1)

(display "test 1: ")
(~>> test
     (string-split _ "\n\n")
     (map count-any-yes)
     (apply +))

(display "test 2: ")
(~> test
    (string-split _ "\n\n"))

;; Puzzle
;(define data (file->list-of-strings "input.txt"))
;
;(display "one: ")
;(~> data
;    (string-split _ "\n\n"))
;
;(display "two: ")
;(~> data
;    (string-split _ "\n\n"))