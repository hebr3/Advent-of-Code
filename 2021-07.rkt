#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2021-07.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")))

(define test
  '("16,1,2,0,4,2,7,1,2,14"))

(define (format-data str)
  (~> str
      car
      (string-split _ ",")
      (map string->number _)))



(~> test
    format-data)

