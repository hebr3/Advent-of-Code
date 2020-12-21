#lang racket
(require threading)
(require racket/match)
(require rackunit)
(require racket/set)

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
(define (rule-line? str) (regexp-match #px"^\\d+:" str))

(define (message-line? str) (regexp-match #px"^[a|b]" str))

(define (parse-rule str)
  (match-let ([(list (list _ rule-num sub-rules))
               (regexp-match* #px"(\\d+): (.+)" str #:match-select values)])
    (list (string->number rule-num) (string-replace sub-rules "\"" ""))))

;; Data
(define data
  (~>> "input/2020-19.txt"
       file->list-of-strings))

(define test
  (~>> "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb
"
       test->list-of-strings))


;; Puzzle
(display "test 1: ")
(~>> test
     (filter rule-line?)
     (map parse-rule)
     (sort _ < #:key car))

(display "one: ")
(~>> data
     (filter rule-line?)
     (map parse-rule)
     (sort _ < #:key car))

;(display "test 2: ")
;(~>> test)
;
;(display "two: ")
;(~>> data)

;; Unit Test
