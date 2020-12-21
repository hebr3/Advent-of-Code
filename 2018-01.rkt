#lang racket
(require threading)

(define test '(1 1 3 3 4 -2 -4 3 3 4 -2 -4 1 1 1))

(define data
  (~> "input.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")
      (map string->number _)))

(display "test: ")
(displayln
 (~> test))

(display "one: ")
(displayln
 (~> data
     (apply + _)))

(display "two: ")
(displayln
 (~> data
     ))


(define (add-to-each n l)
  (cond
    [(empty? l) '()]
    [else (cons (+ (first l) n) (add-to-each n (rest l)))]))