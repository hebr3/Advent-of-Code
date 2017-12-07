#lang racket

(define (diff l)
  (let ([low (apply min l)]
        [high (apply max l)])
    (- high low)))

(apply +
       (map diff
            (map (λ (x) (map string->number x))
                 (map string-split
                      (file->lines "input.txt" #:mode 'text)))))