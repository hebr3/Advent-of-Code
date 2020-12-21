#lang racket
(require threading)
(require racket/match)
(require rackunit)

;; Prep
(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)
      (string-split "\n")))

(define (test->list-of-strings str)
  (~> str
      (string-split "\n")))

;; Struct

;; Functions
(define (rule-line? str) (string-contains? str "or"))
(define (parse-rule str)
  (match-let ([(list (list _ m n i j))
               (regexp-match* #rx".+: (.+)-(.+) or (.+)-(.+)" str #:match-select values)])
    (λ (x) (cons (or (<= (string->number m) x (string->number n))
                     (<= (string->number i) x (string->number j)))
                 x))))

(define (get-rules list-of-strings)
  (~>> list-of-strings
       (filter rule-line?)
       (map parse-rule)))

(define (run-rules list-of-func list-of-nums)
  (for/and ([i list-of-nums])
    (for/or ([f list-of-func])
      (f i))))

(define (ticket-line str) (string-contains? str ","))
(define (parse-ticket str)
  (~>> str
      (string-split _ ",")
      (map string->number)))

;; Data
(define data
  (~>> "input.txt"
       file->list-of-strings))

(define test
  (~>> "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"
       test->list-of-strings))


;; Puzzle
(display "test 1: ")
(~>> test
     (filter rule-line?)
     (map )

(display "one: ")
;(~>> data)

;(display "test 2: ")
;(~>> test)
;
;(display "two: ")
;(~>> data)

;; Unit Test
