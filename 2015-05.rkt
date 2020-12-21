#lang racket
(require threading)
(require racket/match)
(require rackunit)

(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)))

;; Structs

;; Functions
(define (nice-vowels? str)
  (~>> str
       (regexp-match* #px"[aeiou]" _ #:match-select values)
       length
       (<= 3)))

(define (nice-repeats? str)
  (regexp-match #px"(.)(?=\\1)" str))

(define (no-bad-strings? str)
  (~>> (regexp-match* #px"ab|cd|pq|xy" str #:match-select values)
       length
       zero?))

(define (nice? str)
  (and (nice-vowels? str)
       (nice-repeats? str)
       (no-bad-strings? str)))

(define (two-letter-pair? str)
  (~>> (regexp-match* #px"(..).*(?=\\1)" str #:match-select values)
       length
       zero?
       not))

(define (one-letter-repeat? str)
  (~>> (regexp-match* #px"(.).(?=\\1)" str #:match-select values)
       length
       zero?
       not))

(define (nice2? str)
  (and (two-letter-pair? str)
       (one-letter-repeat? str)))
  
;; Data
(define data
  (~>> "input.txt"
       (file->list-of-strings)
       (string-split _ "\n")))

;; Test
(define test
  (~> "ugknbfddgicrmopn
aaa
jchzalrnumimnmhp
haegwjzuvuyypxyu
dvszwmarrgswjxmb
"
      (string-split _ "\n")))

(define test2
  (~> "qjhvhtzxzqqjkmpb
xxyxx
uurcxstgmygtbstg
ieodomkazucvgmuy
"
      (string-split _ "\n")))

;; Puzzle

(display "test 1: ")
(~>> test
     (map nice?)
     (count (λ (x) x)))

(display "one: ")
(~>> data
     (map nice?)
     (count (λ (x) x)))

(display "test2: ")
(~>> test2
     (map nice2?)
     (count (λ (x) x)))

(display "two: ")
(~>> data
     (map nice2?)
     (count (λ (x) x)))

