#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2021-06.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")))

(define test
  '("3,4,3,1,2"))

(define (format-data str)
  (~> str
      car
      (string-split _ ",")
      (map string->number _)))

(define (age-fish num)
  (if (zero? num)
      (list 6 8)
      (list (sub1 num))))

(define (age-all-fish-N-days LoF N)
  (if (zero? N)
      LoF
      (~> LoF
          (map age-fish _)
          flatten
          (age-all-fish-N-days _ (sub1 N)))))

(~> test
    format-data
    (age-all-fish-N-days _ 80)
    length)

(~> data
    format-data
    (age-all-fish-N-days _ 80)
    length)
