#lang racket
(require data/queue)

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

;; Test data
(define data (input->data "input/2024-22.txt"))

(define test "1
10
100
2024
")

;; Structs

;; Helper

(define (secret seed)
  (define (transform-seed seed shift)
    (modulo (bitwise-xor (arithmetic-shift seed shift) seed)
            16777216))
  (let* ([s1 (transform-seed seed 6)]
         [s2 (transform-seed s1 -5)]
         [s3 (transform-seed s2 11)])
    s3))

(define (secret-N seed N)
  (define (iter s c)
    (cond
      [(<= c 0) s]
      [else (iter (secret s) (sub1 c))]))
  (iter seed N))

;; Main Function

(define (part-A input)
  (define numbers (map string->number (string-split input "\n")))
  (for/sum ([seed numbers])
    (secret-N seed 2000)))

(part-A test)
(part-A data)
  
;;

(define (part-B input)
  input)

;(part-B test)
;(part-B data)
