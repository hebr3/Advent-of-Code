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
  (define (helper val acc)
    (if (string=? "" val)
        (list 0 acc)
        (append (list (+ (string->number val) (first acc))) (rest acc))))
  (~> L
      (foldl helper (list 0) _)
      flatten
      (apply max _)))

(part-A test)
(part-A data)

(define (part-B L)
  (define (helper val acc)
    (if (string=? "" val)
        (list 0 acc)
        (append (list (+ (string->number val) (first acc))) (rest acc))))
  (~> L
      (foldl helper (list 0) _)
      flatten
      (sort _ >)
      (take _ 3)
      (apply + _)))

(part-B test)
(part-B data)
