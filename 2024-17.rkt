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

;; (part-A test0)
;; (part-A test1)
;; (part-A test2)
;; (part-A test3)
;; (part-A test4)
;; (part-A test5)
;data
; (part-A data)
;;

(define test6 "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
")

(define (part-B input)
  (define lines (filter non-empty-string? (string-split input "\n")))
  (define numbers (for/list ([line lines]) (map string->number (regexp-match* #px"\\d+" line))))
  (match-define (list (list A) (list B) (list C) Program) numbers)

  (define (iter a b c pt o)
    (println (list a b c pt o Program))
    (println (list (length o) (length Program)))
    (cond
      [(zero? a) o]
      [else
       (define b* (modulo a 8)) ; step 1 (2,4)
       (define b** (bitwise-xor b* 1)) ; step 2 (1,1)
       (define c* (floor (/ a (expt 2 b**)))) ; step 3 (7,5)
       (define a* (floor (/ a 8))) ; step 4 (0,3)
       (define b*** (bitwise-xor b** 4)) ; step 5 (1,4)
       (define b**** (bitwise-xor b*** c*)) ; step 6 (4,4)
       (define o* (flatten (list o (modulo b**** 8)))) ; step 7 (5,5)    
       (define pt* (if (zero? a) (+ 2 pt) 0)) ; step 6 (3,0)
       (iter (floor (/ a 8)) b**** c* pt* o*)]))


  (for ([i 16]) (iter i 0 0 0 '()) (displayln ""))

  (for ([i (list (+ (expt 2 45)))])
    (iter (+ i) 0 0 0 '())
    (displayln ""))

  (for ([j 16])
    (for ([i (list (+ (* j (expt 8 6))
                      (* 1 (expt 8 7))
                      (* 1 (expt 8 8))
                      (* 1 (expt 8 9))
                      (* 1 (expt 8 10))
                      (* 1 (expt 8 11))
                      (* 1 (expt 8 12))
                      (* 1 (expt 8 13))
                      (* 1 (expt 8 14))
                      (* 1 (expt 8 15))))])
      (iter (+ i) 0 0 0 '())
      (displayln j))))

(part-B data)

;(define tmp (part-B test6))
;; (define tmp (part-B data))
;; 
;; 
;; (display "x = np.array([")
;; (for ([i tmp][j (in-naturals)])
;;   (display (format "~a," j)))
;; (displayln "])")
;; 
;; (display "y = np.array([")
;; (for ([i tmp][j (in-naturals)])
;;   ;  (when (zero? (modulo j 3))
;;   (display (format "~a," i)))
;; (displayln "])")

;35184381536810
"id:35184381536810 A:2097152 B:524289 C:524288 output:12 pt:(2 4 1 1 7 5 0) op:5 combo:5"
