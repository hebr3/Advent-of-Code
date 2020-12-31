#lang racket
(require threading)

(define data
  (~> "input/2020-01.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")
      (map string->number _)))

(define (find-target-pair target)
  (λ (list-of-numbers)
    (for/or ([i list-of-numbers])
      (let ([j (- target i)])
        (and (member j list-of-numbers)
             (* i j))))))

(define find-2020-pair (find-target-pair 2020))

(define (find-2020-triple list-of-numbers)
  (define (iter target lon)
    (for/or ([i lon])
      (and (member (- target i) lon)
           (* (- 2020 target) i (- target i)))))
  (or (iter (- 2020 (first list-of-numbers)) (rest list-of-numbers))
      (find-2020-triple (rest list-of-numbers))))

(display "one: ")
(displayln 
 (~> data
     find-2020-pair))

(display "two: ")
(displayln
 (~> data
     find-2020-triple))
