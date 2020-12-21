#lang racket
(require threading)
(require racket/match)
(require rackunit)

(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)))

;; Struct
(struct location [heading x y] #:transparent)

;; Constant
(define start (location 'N 0 0))

;; Functions
(define (convert-dir dir)
  (string-append (substring dir 0 1)
                 (make-string (string->number (substring dir 1)) #\.)))

(define (walk-string h loc list-of-dirs)
  (if (empty? list-of-dirs)
      '()
      (cond
        [(char
     

;; Test
(define test4 "R8, R4, R4, R8\n")

(display "test 4: ")
(~> test4
    string-trim
    (string-split _ ", ")
    (map convert-dir _)
    (apply string-append _))

;; Puzzle
(define data (file->list-of-strings "input.txt"))

(display "two: ")
;(~> data
;    string-trim
;    (string-split _ ", "))
