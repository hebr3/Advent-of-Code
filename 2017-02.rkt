#lang racket
(require threading)

(define (string-split-on-return str)
  (string-split str "\n"))

(define (string-split-on-tab str)
  (string-split str "\t"))

(define (list-of-string->list-of-numbers los)
  (map string->number los))

(define (max-min list-of-numbers)
  (let ([S (sort list-of-numbers >)])
    (- (first S)
       (last S))))

(define (multiples? x y)
  (or (zero? (modulo x y))
      (zero? (modulo y x))))

(define (find-multiples list-of-numbers)
  (define (iter head tail)
    (for/or ([i tail])
      (if (multiples? head i)
          (max (/ head i) (/ i head))
          #f)))
  (or (iter (first list-of-numbers)
            (rest list-of-numbers))
      (find-multiples (rest list-of-numbers))))

(define data
  (~> "input.txt"
      open-input-file
      (read-line _ 'return)
      string-split-on-return
      (map string-split-on-tab _)
      (map list-of-string->list-of-numbers _)))

(display "one: ")
(displayln
 (~> data
     (map max-min _)
     (apply + _)))

(display "two: ")
(displayln
 (~> data
     (map find-multiples _)
     (apply + _)))