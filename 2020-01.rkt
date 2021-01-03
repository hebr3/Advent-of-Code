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
  (for*/first ([i list-of-numbers]
               [j (rest list-of-numbers)]
               [k (rest (rest list-of-numbers))]
               #:when (zero? (- 2020 i j k)))
    (* i j k)))

(display "one: ")
(displayln 
 (~> data
     find-2020-pair))

(display "two: ")
(displayln
 (~> data
     find-2020-triple
     time))

"cpu time: 109 real time: 114 gc time: 31"