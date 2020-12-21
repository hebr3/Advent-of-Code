#lang racket
(require threading)
(require racket/match)
(require rackunit)

(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)))

;; Functions
(define (count-repeat list-of-list-of-char)
  (if (empty? (rest list-of-list-of-char))
      (length (first list-of-list-of-char))
      (for/sum ([i (first list-of-list-of-char)])
        (if (for/and ([j (rest list-of-list-of-char)])
              (member i j))
            1
            0))))

;; Test
(define test
  "abc

a
b
c

ab
ac

a
a
a
a

b")

(check-eq? (identity 1)
           1)
(check-eq? (identity 1)
           1)
(check-eq? (identity 1)
           1)

(display "test 1: ")
(apply +
       (~> test
           (string-split _ "\n\n")
           (map (λ (x) (string-replace x "\n" "")) _)
           (map string->list _)
           (map (λ (x) (sort x char<?)) _)
           (map (λ (x) (group-by identity x)) _)
           (map length _)))

(display "test 2: ")
(apply +
       (~> test
           (string-split _ "\n\n")
           (map (λ (x) (string-split x "\n")) _)
           (map (λ (x) (map (λ (xx) (string->list xx)) x)) _)
           (map count-repeat _)))

;; Puzzle
(define data (file->list-of-strings "input.txt"))

(display "one: ")
(apply +
       (~> data
           (string-split _ "\n\n")
           (map (λ (x) (string-replace x "\n" "")) _)
           (map string->list _)
           (map (λ (x) (sort x char<?)) _)
           (map (λ (x) (group-by identity x)) _)
           (map length _)))

(display "two: ")
(apply +
       (~> data
           (string-split _ "\n\n")
           (map (λ (x) (string-split x "\n")) _)
           (map (λ (x) (map (λ (xx) (string->list xx)) x)) _)
           (map count-repeat _)))