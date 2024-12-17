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

  (define output '())

  (define (iter a b c pt o)
    (define (lookup v)
      (match v
        [0 0][1 1][2 2][3 3]
        [4 A][5 B][6 C][7 (error "Shouldn't happen")]))
    (when (< pt (length Program))
      (define op (list-ref Program pt))
      (define combo (list-ref Program (add1 pt)))
      (match op
        [0 (set! A (floor (/ A (expt 2 (lookup combo)))))]
        [1 (set! B (bitwise-xor B combo))]
        [2 (set! B (modulo (lookup combo) 8))]
        [3 (iter a b c (if (zero? A) (+ 2 pt) combo) o)]
        [4 (set! B (bitwise-xor B C))]
        [5 (set! output (flatten (list output (modulo (lookup combo) 8))))]
        [6 (set! B (floor (/ A (expt 2 (lookup combo)))))]
        [7 (set! C (floor (/ A (expt 2 (lookup combo)))))])
      (iter a b c (+ 2 pt) o)))
  (iter A B C 0 '())
  
  (for ([o output])
    (display (format "~a," o)))
  
  (display " : ")
  (displayln (list A B C)))

;; (part-A test0)
;; (part-A test1)
;; (part-A test2)
(part-A test3)
;; (part-A test4)
;; (part-A test5)
;; (part-A data)
(displayln "4,2,5,6,7,7,7,7,3,1,0")
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