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
         (let ([len (length L)])
           (for/list ([i (sub1 len)])
             (< (list-ref L i) (list-ref L (add1 i)))))))

(count-increases test)
(count-increases data)

(define (count-increases-three L)
  (count (λ (x) x)
         (let ([len (length L)])
           (for/list ([i (sub1 len)])
             (< (list-ref L i) (list-ref L (add1 i)))))))

(count-increases-three test)
(count-increases-three data)

;(define (find-target-pair target)
;  (λ (list-of-numbers)
;    (for/or ([i list-of-numbers])
;      (let ([j (- target i)])
;        (and (member j list-of-numbers)
;             (* i j))))))
;
;(define find-2020-pair (find-target-pair 2020))
;
;(define (find-2020-triple list-of-numbers)
;  (define i (first list-of-numbers))
;  (define find-2020-minus-first (find-target-pair (- 2020 i)))
;  (or (let ([jk (find-2020-minus-first (rest list-of-numbers))])
;        (and jk (* i jk)))
;      (find-2020-triple (rest list-of-numbers))))
;
;(display "one: ")
;(displayln 
; (~> data
;     find-2020-pair))
;
;(display "two: ")
;(displayln
; (~> data
;     find-2020-triple))
