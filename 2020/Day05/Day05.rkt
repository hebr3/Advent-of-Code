#lang racket
(require threading)
(require racket/match)
(require rackunit)

(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)))

;; Functions
(define (boarding-pass->seat-id str)
  (match-let ([(list a b c d e f g x y z)
               (string->list str)])
    (+ (~> (list a b c d e f g)
           (map (λ (x) (if (char=? #\F x) #\0 #\1)) _)
           (append '(#\# #\b) _) 
           (apply string _)
           string->number
           (* 8))
       (~> (list x y z)
           (map (λ (x) (if (char=? #\R x) #\1 #\0)) _)
           (append '(#\# #\b) _) 
           (apply string _)
           string->number))))

(define (find-missing-seat list-of-seats)
  (for/or ([i #b1111111111])
    (and (member (sub1 i) list-of-seats)
         (member (add1 i) list-of-seats)
         (not (member i list-of-seats))
         i)))

;; Test
(define test
  "FBFBBFFRLR
FFFBBBFRRR
BBFFBBFRLL")

(check-eq? (boarding-pass->seat-id "FBFBBFFRLR")
           357)
(check-eq? (boarding-pass->seat-id "FFFBBBFRRR")
           119)
(check-eq? (boarding-pass->seat-id "BBFFBBFRLL")
           820)

(display "test 1: ")
(~> test
    (string-split _ "\n")
    (map boarding-pass->seat-id _))

;; Puzzle
(define data (file->list-of-strings "input.txt"))

(display "one: ")
(~> data
    (string-split _ "\n")
    (map boarding-pass->seat-id _)
    (sort _ >)
    first)

(display "two: ")
(~> data
    (string-split _ "\n")
    (map boarding-pass->seat-id _)
    find-missing-seat)
