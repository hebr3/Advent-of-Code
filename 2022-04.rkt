#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2022-04.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")
      ))

(define test '("2-4,6-8"
               "2-3,4-5"
               "5-7,7-9"
               "2-8,3-7"
               "6-6,4-6"
               "2-6,4-8"))

(define (split-input str)
  (map string->number (string-split str #px"(,|-)")))

(define (contains? LoN)
  (match-let ([(list aLow aHigh bLow bHigh) LoN])
    (or (<= aLow bLow bHigh aHigh)
        (<= bLow aLow aHigh bHigh))))

(define (part-A L)
  (~>> L
       (map split-input)
       (count contains?)))

(part-A test)
(part-A data)

;;---

(define (contains-2? LoN)
  (match-let ([(list aLow aHigh bLow bHigh) LoN])
    (or (<= aLow bLow aHigh)
        (<= aLow bHigh aHigh)
        (<= bLow aLow bHigh)
        (<= bLow aHigh bHigh))))

(define (part-B L)
  (~>> L
       (map split-input)
       (count contains-2?)))

(part-B test)
(part-B data)
