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
(define (line->num str)
  (define L? (char=? (string-ref str 0) #\L))
  (define N (string->number (substring str 1)))
  (cond
    [L? (- 0 N)]
    [else N]))

;; Main Function
(define (part-A input)
  (define lines (string-split input "\n"))
  (define numbers (map line->num lines))
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
      (define NUM (line->num line))
      (for ([i (in-range (abs NUM))])
        (cond
          [(< NUM 0)
           (set! dial (modulo (sub1 dial) 100))]
          [else
           (set! dial (modulo (add1 dial) 100))])
        (when (zero? dial)
          (set! zeroes (add1 zeroes)))))
    zeroes))

(part-B test)
(part-B data)
