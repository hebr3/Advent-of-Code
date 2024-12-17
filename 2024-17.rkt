#lang racket

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

;; Test data
(define data (input->data "input/2024-17.txt"))

(define test0 "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
")
(define test1 "Register A: 0
Register B: 0
Register C: 9

Program: 2,6
")
(define test2 "Register A: 10
Register B: 0
Register C: 0

Program: 5,0,5,1,5,4
")
(define test3 "Register A: 2024
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
")
(define test4 "Register A: 0
Register B: 29
Register C: 0

Program: 1,7
")
(define test5 "Register A: 0
Register B: 2024
Register C: 43690

Program: 4,0
")
;; Structures

;; Helper Functions

;; Main Function
(define (part-A input)
  (define lines (filter non-empty-string? (string-split input "\n")))
  (define numbers (for/list ([line lines]) (map string->number (regexp-match* #px"\\d+" line))))
  (match-define (list (list A) (list B) (list C) Program) numbers)

  (define (iter a b c pt o)
    (define (lookup v)
      (match v
        [0 0][1 1][2 2][3 3]
        [4 a][5 b][6 c][7 (error "Shouldn't happen")]))
    (cond
      [(< pt (length Program))
       (define op (list-ref Program pt))
       (define combo (list-ref Program (add1 pt)))
       (define pt* (+ 2 pt))
       (match op
         [0 (iter (floor (/ a (expt 2 (lookup combo)))) b c pt* o)]
         [1 (iter a (bitwise-xor b combo) c pt* o)]
         [2 (iter a (modulo (lookup combo) 8) c pt* o)]
         [3 (iter a b c (if (zero? a) pt* combo) o)]
         [4 (iter a (bitwise-xor b c) c pt* o)]
         [5 (iter a b c pt* (flatten (list o (modulo (lookup combo) 8))))]
         [6 (iter a (floor (/ a (expt 2 (lookup combo)))) c pt* o)]
         [7 (iter a b (floor (/ a (expt 2 (lookup combo)))) pt* o)])]
      [else (list a b c o)]))
  (iter A B C 0 '()))

(part-A test0)
(part-A test1)
(part-A test2)
(part-A test3)
(part-A test4)
(part-A test5)
(part-A data)
;;

(define test6 "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
")

(define (part-B input)
  input)

;(part-B test0)
;(part-B test6)
;(part-B data)