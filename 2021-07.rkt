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

(define (triangle n)
  (* n (add1 n) 1/2))

(define (solve LoN)
  (let* ([max (apply max LoN)]
         [min-fuel +inf.0])
    (for ([m max])
      (define tmp
        (for/sum ([i LoN])
          (abs (- i m))))
      (when (< tmp min-fuel)
        (set! min-fuel tmp)))
    min-fuel))

(define (solve2 LoN)
  (let* ([max (apply max LoN)]
         [min-fuel +inf.0])
    (for ([m max])
      (define tmp
        (for/sum ([i LoN])
          (triangle (abs (- i m)))))
      (when (< tmp min-fuel)
        (set! min-fuel tmp)))
    min-fuel))

(~> test
    format-data
    solve)
(~> data
    format-data
    solve)
(~> test
    format-data
    solve2)
(~> data
    format-data
    solve2)