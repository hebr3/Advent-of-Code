#lang racket
(require threading)
(require racket/match)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2023-06.txt"))

(define test "Time:      7  15   30
Distance:  9  40  200")

;;

(define (calc-dist t d)
  (for/sum ([i t])
    (if (< d (* i (- t i))) 1 0)))

(define (part-A L)
  (let* ([lines (string-split L "\n")]
         [times (map string->number (rest (string-split (first lines) #px"\\s+")))]
         [dists (map string->number (rest (string-split (second lines) #px"\\s+")))])
    (for/product ([t times][d dists])
      (calc-dist t d))))

(part-A test)
(part-A data)

;;


(define (part-B L)
  (let* ([lines (string-split L "\n")]
         [time (string->number (string-join (rest (string-split (first lines) #px"\\s+")) ""))]
         [dist (string->number (string-join (rest (string-split (second lines) #px"\\s+")) ""))])
    (calc-dist time dist)))

(part-B test)
(part-B data)
