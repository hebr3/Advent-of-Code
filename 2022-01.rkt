#lang racket
(require threading)

(define data
  (~> "input/2022-01.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")
      ))

(define test '("1000"
"2000"
"3000"
""
"4000"
""
"5000"
"6000"
""
"7000"
"8000"
"9000"
""
"10000"))

(define (part-A L)
  42)

(part-A test)
(part-A data)

(define (part-B L)
  42)

(part-B test)
(part-B data)
