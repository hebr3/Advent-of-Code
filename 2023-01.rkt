#lang racket
(require threading)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2023-01.txt"))

(define test "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(define (part-A L)
  (~> L
      (string-split "\n")
      (map (λ (s) (regexp-match* #px"\\d" s)) _)
      (map (λ (l) (list (first l) (last l))) _)
      (map (λ (l) (string-join l "")) _)
      (map (λ (s) (string->number s)) _)
      (apply + _)
      ))

(part-A test)
(part-A data)

;;

(define lookup-hash
  (make-hash (list (cons "one" 1)
                   (cons "two" 2)
                   (cons "three" 3)
                   (cons "four" 4)
                   (cons "five" 5)
                   (cons "six" 6)
                   (cons "seven" 7)
                   (cons "eight" 8)
                   (cons "nine" 9)
                   (cons "1" 1)
                   (cons "2" 2)
                   (cons "3" 3)
                   (cons "4" 4)
                   (cons "5" 5)
                   (cons "6" 6)
                   (cons "7" 7)
                   (cons "8" 8)
                   (cons "9" 9)
                   (cons "0" 0))))

(define exp #px"\\d|one|two|three|four|five|six|seven|eight|nine")

(define test2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(define (part-B L)
  (~> L
      (string-split "\n")
      (map (λ (s) (regexp-match* exp s)) _)
      (map (λ (l) (map (λ (ll) (hash-ref lookup-hash ll)) l)) _)
      (map (λ (l) (+ (* 10 (first l)) (last l))) _)
      (apply + _)
      ))

(part-B test2)
(part-B data)
