#lang racket
(require threading)

(define test-input '("b inc 5 if a > 1"
                     "a inc 1 if b < 5"
                     "c dec -10 if a >= 1"
                     "c inc -20 if c == 10"))

(define input
;  (~> test-input
;      (map string-split _)))
  (~> "input.txt"
      file->lines
      (map string-split _)))


  
; Find all registers in the input and set to 0
(define registers
  (make-hash
   (for/list ([i (map car input)])
     (cons i 0))))

(define results '())

(define (parse-func f)
  (if (string=? f "inc") + -))

(define (parse-pred p)
  (match p
    [">" >]
    [">=" >=]
    ["<" <]
    ["<=" <=]
    ["==" =]
    ["!=" (λ (x y) (not (= x y)))]))

(define (parse-input lst)
  (let* ([reg (list-ref lst 0)]
         [reg* (hash-ref registers reg)]
         [func (parse-func (list-ref lst 1))]
         [n (string->number (list-ref lst 2))]
         [a (list-ref lst 4)]
         [a* (hash-ref registers a)]
         [pred (list-ref lst 5)]
         [b (string->number (list-ref lst 6))])
    (when ((parse-pred pred) a* b)
      ;(printf "run: ~a ~a ~a\n" a pred b))
      (hash-set! registers reg (func reg* n))
      (set! results (cons (func reg* n) results)))))

;registers
(for ([i input])
  (parse-input i))
(apply max (hash-values registers))
(apply max results)