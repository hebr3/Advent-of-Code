#lang racket
(require threading)
(require racket/match)
(require rackunit)

(require file/md5)

(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)))

;; Structs

;; Functions
(define (leading-00000? str)
  (~> str
      md5
      (subbytes 0 5)
      bytes->string/locale
      (string=? "00000")))

(define (leading-000000? str)
  (~> str
      md5
      (subbytes 0 6)
      bytes->string/locale
      (string=? "000000")))

(define (mine-input str)
  (for/or ([i (in-naturals)]
             #:when (leading-00000? (string-join (list str (number->string i)) "")))
    i))

(define (mine-input2 str)
  (for/or ([i (in-naturals)]
             #:when (leading-000000? (string-join (list str (number->string i)) "")))
    i))

;; Data
(define data "ckczppom")

;; Test
(define test
  (~> "abcdef
pqrstuv
"
      (string-split _ "\n")))

;; Puzzle

(display "test 1: ")
(~>> test
     (map mine-input))

(display "one: ")
(~>> data
     mine-input)

(display "test2: ")
(~>> test)

(display "two: ")
(~>> data
     mine-input2)
