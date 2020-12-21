#lang racket
(require threading)
(require racket/match)
(require rackunit)

;; Prep

;; Struct

;; Hash
(define test-spoken (make-hash))
(define spoken (make-hash))

;; Functions
(define (test-initialize lon)
  (for ([i (length lon)])
    (hash-set! test-spoken (list-ref lon i) i)))

(define (test-calculate-next last-num idx)
  (when (< idx 11)
    (let ([last-occurance (hash-ref test-spoken last-num #f)])
      (if last-occurance
          (begin
            (displayln (list "occur" last-num idx last-occurance))
            (hash-set! test-spoken (- idx last-occurance) idx)
            (test-calculate-next (- idx last-occurance) (add1 idx)))
          (begin
            (displayln (list "no" last-num idx last-occurance))
            (hash-set! test-spoken 0 idx)
            (test-calculate-next 0 (add1 idx)))))))
        

;; Data
(define data
  (~> "0,1,4,13,15,12,16"
      (string-split ",")
      (map string->number _)))

(define test
  (~> "0,3"
      (string-split ",")
      (map string->number _)))

;; Puzzle
(display "test 1: ")
(test-initialize test)
test-spoken
(test-calculate-next 6 4)
test-spoken


(display "one: ")
(~>> data)

;(display "test 2: ")
;(~>> test)
;
;(display "two: ")
;(~>> data)

;; Unit Test
