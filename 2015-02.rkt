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
(define (parse-box str)
  (~>> str
       (string-split _ "x")
       (map string->number)
       (sort _ <)))

(define (calculate-paper box)
  (match-let ([(list l w h) box])
    (+ (* 3 l w) (* 2 w h) (* 2 h l))))

(define (calculate-ribbon box)
  (match-let ([(list l w h) box])
    (+ l l w w
       (* l w h))))

;; Data
(define data
  (~>> "input.txt"
       (file->list-of-strings)
       (string-split _ "\n")))

;; Test
(define test
  (~> "2x3x4
1x1x10
"
      (string-split _ "\n")))

;; Puzzle

(display "test 1: ")
(~>> test
     (map parse-box)
     (map calculate-paper))

(display "one: ")
(~>> data
     (map parse-box)
     (map calculate-paper)
     (apply +))

(display "test2: ")
(~>> test
     (map parse-box)
     (map calculate-ribbon))

(display "two: ")
(~>> data
     (map parse-box)
     (map calculate-ribbon)
     (apply +))

