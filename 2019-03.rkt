#lang racket

(define input
  (map (λ (x) (string-split x ","))
       (file->lines "input.txt")))

(define (helper s)
  (list (substring s 0 1) (string->number (substring s 1))))

(list (map helper (car input))
      (map helper (cadr input)))

(define (add-dir dir lst)
  (let ([acc '()])
    (cond
      [(string? (car dir) "U")
       (for ([i (cadr dir)])
         (set! acc (cons 