#lang racket
(require threading)

(define data
  (~> "input/2021-01.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")
      (map string->number _)
      ))

(define test '(199 200 208 210 200 207 240 269 260 263))

(define (count-increases L)
  (count (λ (x) x)
         (for/list ([i L][j (cdr L)]) (< i j))))

(count-increases test)
(count-increases data)

(define (count-increases-three L)
  (count (λ (x) x)
         (for/list ([i L][j (cddr L)]) (< i j))))

(count-increases-three test)
(count-increases-three data)
