#lang racket
(require threading)
(require racket/match)

;; Prep
(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)
      (string-split "\n")))

(define (test->list-of-strings str)
  (string-split str "\n"))

;; Structs

;; Functions
(define (add-ends L)
  (append (list 0) L (list (+ 3 (last L)))))

(define (find-differences L)
  (for/list ([i L][j (rest L)])
    (- j i)))

(define (ones*threes L)
  (define (one? x) (= 1 x))
  (define (three? x) (= 3 x))
  (* (count one? L)
     (count three? L)))

;; Data
(define test1
  (test->list-of-strings "16
10
15
5
1
11
7
19
6
12
4"))

(define test2
  (test->list-of-strings "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"))
(define data (file->list-of-strings "input.txt"))

;; Puzzle
(display "test 1: ")
(~>> test1
     (map string->number)
     (sort _ <)
     add-ends
     find-differences
     ones*threes)
(display "test 2: ")
(~>> test2
     (map string->number)
     (sort _ <)
     add-ends
     find-differences
     ones*threes)
(display "one: ")
(~>> data
     (map string->number)
     (sort _ <)
     add-ends
     find-differences
     ones*threes)

;(display "test 2: ")
;(~>> test)
;
;(display "two: ")
;(~>> data)
