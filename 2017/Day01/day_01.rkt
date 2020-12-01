#lang racket
(require threading)

(define data
  (~> "input.txt"
      open-input-file
      read-line
      (string-split _ "")
      (map string->number _)
      (filter (λ (x) x) _)))

(display "one: ")
(displayln 
 (let ([N (length data)])
   (for/sum ([i (range N)])
     (let ([x (list-ref data i)]
           [y (list-ref data (modulo (add1 i) N))])
       (if (= x y)
           x
           0)))))

(display "two: ")
(displayln
 (let* ([N (length data)]
        [N/2 (/ N 2)])
   (for/sum ([i (range N)])
     (let ([x (list-ref data i)]
           [y (list-ref data (modulo (+ i N/2) N))])
       (if (= x y)
           x
           0)))))