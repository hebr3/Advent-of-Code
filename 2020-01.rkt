#lang racket
(require threading)

(define data
  (~> "input.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")
      (map string->number _)))

(define (find-2020-pair list-of-numbers)
  (define (iter head tail)
    (for/or ([i tail])
      (if (zero? (- 2020 head i))
          (* head i)
          #f)))
  (or (iter (first list-of-numbers) (rest list-of-numbers))
      (find-2020-pair (rest list-of-numbers))))

(define (find-2020-triple list-of-numbers)
  (define (iter2 one two rst)
    (for/or ([i rst])
      (if (zero? (- 2020 one two i))
          (* one two i)
          #f)))
  (define (iter head tail)
    (for/or ([i (range (length tail))])
      (iter2 head (list-ref tail i) (drop tail i))))
  (or (iter (first list-of-numbers)
            (rest list-of-numbers))
      (find-2020-triple (rest list-of-numbers))))

(display "one: ")
(displayln 
 (~> data
     find-2020-pair))

(display "two: ")
(displayln
 (~> data
     find-2020-triple))