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
(define (update-location turn dir)
  (let ([t (substring turn 0 1)]
        [d (string->number (substring turn 1))])
    (match dir
      [(location 'N x y)
       (if (string=? t "R")
           (location 'E (+ x d) y)
           (location 'W (- x d) y))]
      [(location 'S x y)
       (if (string=? t "R")
           (location 'W (- x d) y)
           (location 'E (+ x d) y))]
      [(location 'E x y)
       (if (string=? t "R")
           (location 'S x (- y d))
           (location 'N x (+ y d)))]
      [(location 'W x y)
       (if (string=? t "R")
           (location 'N x (+ y d))
           (location 'S x (- y d)))])))

(define (read-dirs list-of-dirs)
  (~> list-of-dirs
      (string-split _ ", ")
      (foldl update-location start _)))

(define (loc->dist loc)
  (match-let ([(location _ x y) loc])
    (+ (abs x) (abs y))))

;; Test
(check-equal? (update-location "R3" start)
              (location 'E 3 0))
(check-equal? (update-location "L3" start)
              (location 'W -3 0))
(check-equal? (update-location "R3" (location 'E 0 0))
              (location 'S 0 -3))
(check-equal? (update-location "L3" (location 'E 0 0))
              (location 'N 0 3))

(define test1 "R2, L3")
(define test2 "R2, R2, R2")
(define test3 "R5, L5, R5, R3")

(check-equal? (read-dirs test1)
              (location 'N 2 3))
(check-equal? (read-dirs test2)
              (location 'W 0 -2))
(check-equal? (read-dirs test3)
              (location 'S 10 2))

(display "test 1: ")
(~> test1
    read-dirs
    loc->dist)

(display "test 2: ")
(~> test2
    read-dirs
    loc->dist)

(display "test 3: ")
(~> test3
    read-dirs
    loc->dist)

;; Puzzle
(define data (file->list-of-strings "input.txt"))

(display "one: ")
(~> data
    string-trim
    read-dirs
    loc->dist)

(display "two: ")
(~> data
    string-trim)
