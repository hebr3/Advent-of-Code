#lang racket
(require threading)

(define (diff l)
  (let ([low (apply min l)]
        [high (apply max l)])
    (- high low)))

(apply +
       (map diff
            (map (λ (x) (map string->number x))
                 (map string-split
                      (file->lines "input.txt" #:mode 'text)))))

(define (check lst)
  (for/or ([i (length lst)])
    (let* ([s (sort lst <)]
           [t (list-ref s i)]
           [r (drop s (add1 i))])
      (for/or ([j r])
        (if (zero? (modulo j t))
            (/ j t)
            #f)))))

(apply +
       (map check
            (map (λ (x) (map string->number x))
                 (map string-split
                      (file->lines "input.txt" #:mode 'text)))))