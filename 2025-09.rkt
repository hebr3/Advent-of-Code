#lang racket

(require rackunit)
(require racket/set)

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))
    #:mode 'text))

;; Test data
(define data (input->data "input/2025-09.txt"))

(define test "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

;; Helper Function
(struct point [x y] #:transparent)

(define (area p1 p2)
  (match-let ([(point x1 y1) p1]
              [(point x2 y2) p2])
    (* (add1 (abs (- x1 x2)))
       (add1 (abs (- y1 y2))))))

(define (calc-areas pairs)
  (for/list ([pair pairs])
    (match-let ([(list p1 p2) pair])
      (area p1 p2))))

(define (parse-input input)
  (for/list ([line (string-split input "\n")])
    (match-define (list x y) (string-split line ","))
    (point (string->number x) (string->number y))))

;; Main Function
(define (part-A input)
  (define points (parse-input input))
  (define pairs (combinations points 2))
  (define areas (calc-areas pairs))
  (apply max areas))

(check-equal? (part-A test) 50)

(part-A data)

;;

(define (part-B input)
  input)

;(check-equal? (part-B test) 0)

;(part-B data)
