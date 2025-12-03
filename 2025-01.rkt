#lang racket

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

;; Test data
(define data (input->data "input/2025-01.txt"))

(define test "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

;; Helper Function
(define (line->num str)
  (define L? (char=? (string-ref str 0) #\L))
  (define N (string->number (substring str 1)))
  (cond
    [L? (- 0 N)]
    [else N]))

(struct Dial [value count] #:transparent)

(define (turn-dial num dial)
  (let* ([old-value (Dial-value dial)]
         [old-count (Dial-count dial)]
         [new-value (modulo (+ old-value num) 100)]
         [new-count (if (zero? new-value) (add1 old-count) old-count)])
    (Dial new-value new-count)))

(define (explode-list-of-numbers lon)
  (flatten
   (for/list ([num lon])
     (make-list (abs num) (if (negative? num) -1 1)))))

;; Main Function
(define (part-A input)
  (define lines (string-split input "\n"))
  (define numbers (map line->num lines))
  (foldl turn-dial (Dial 50 0) numbers))

(part-A test)
(part-A data)

;;

(define (part-B input)
  (define lines (string-split input "\n"))
  (define numbers (map line->num lines))
  (define exploded (explode-list-of-numbers numbers))
  (foldl turn-dial (Dial 50 0) exploded))

(part-B test)
(part-B data)
