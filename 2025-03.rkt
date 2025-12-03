#lang racket

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

;; Test data
(define data (input->data "input/2025-03.txt"))

(define test "987654321111111
811111111111119
234234234234278
818181911112111")

;; Helper Function
(define (string->numbers str)
  (filter (λ (x) x)
          (map string->number
               (string-split str ""))))

(define (find-largest-pair str)
  (define numbers (string->numbers str))
  (define M1 (apply max (take numbers (sub1 (length numbers)))))
  (define rest-of-numbers (drop (member M1 numbers) 1))
  (define M2 (apply max rest-of-numbers))
  (+ (* 10 M1) M2))

;; Main Function
(define (part-A input)
  (define lines (string-split input "\n"))
  (for/sum ([line lines])
    (find-largest-pair line)))

(part-A test)
(part-A data)

;;

(define (part-B input)
  input)

;(part-B test)
;(part-B data)
