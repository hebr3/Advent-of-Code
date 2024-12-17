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

  (define (lookup v)
    (match v
      [0 0][1 1][2 2][3 3]
      [4 A][5 B][6 C][7 (error "Shouldn't happen")]))

  (define (adv combo) ; 0
    (set! A (floor (/ A (expt 2 (lookup combo))))))
  (define (bxl combo) ; 1
    (set! B (bitwise-xor B combo)))
  (define (bst combo) ; 2
    (set! B (modulo (lookup combo) 8)))
  (define (jnz combo) ; 3
    combo)
  (define (bxc combo) ; 4
    (set! B (bitwise-xor B C)))
  (define (out combo) ; 5
    (display (modulo (lookup combo) 8))
    (display ","))
  (define (bdv combo) ; 6
    (set! B (floor (/ A (expt 2 (lookup combo))))))
  (define (cdv combo) ; 7
    (set! C (floor (/ A (expt 2 (lookup combo))))))

  (define (iter pt)
    (when (< pt (length Program))
      (define op (list-ref Program pt))
      (define combo (list-ref Program (add1 pt)))
      (cond
        [(= op 3)
         (cond
           [(zero? A) (iter (+ 2 pt))]
           [else (iter combo)])]
        [else
         (match op
           [0 (adv combo)]
           [1 (bxl combo)]
           [2 (bst combo)]
           [4 (bxc combo)]
           [5 (out combo)]
           [6 (bdv combo)]
           [7 (cdv combo)])])
      (iter (+ 2 pt))))
  (iter 0)
  
  (display " : ")
  (displayln (list A B C)))

;; (part-A test0)
;; (part-A test1)
;; (part-A test2)
 (part-A test3)
;; (part-A test4)
;; (part-A test5)
;; (part-A data)

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
(part-B data)