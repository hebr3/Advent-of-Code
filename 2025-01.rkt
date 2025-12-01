#lang racket

(require data/heap)

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
(define (str->num str)
  (define N (string->number (substring str 1)))
  (if (string=? (substring str 0 1) "R")
      N
      (- 0 N)))

;; Main Function
(define (part-A input)
  (define lines (string-split input "\n"))
  (define numbers (map str->num lines))
  (let ([result 50][vals '()])
    (for ([n numbers])
      (set! result (modulo (+ result n) 100))
      (set! vals (cons result vals)))
    (count zero? vals)))

(part-A test)
(part-A data)

;;

(define (part-B input)
  (define lines (string-split input "\n"))
  (let ([dial 50][zeroes 0])
    (for ([line lines])
      (define D (substring line 0 1))
      (define NUM (string->number (substring line 1)))
      (cond
        [(string=? "L" D)
         (for ([i NUM])
           (set! dial (modulo (sub1 dial) 100))
           (when (zero? dial)
             (set! zeroes (add1 zeroes))))]
        [else
         (for ([i NUM])
           (set! dial (modulo (add1 dial) 100))
           (when (zero? dial)
             (set! zeroes (add1 zeroes))))]))
    zeroes))

(part-B test)
(part-B data)
