#lang racket
(require threading)
(require racket/match)

(define test
  "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

(define data
  (~> "input.txt"
      open-input-file
      (read-line _ 'return-linefeed)))

(define (valid-password? str)
  (match-let ([(list a b c d e) (string-split str #px"\\-|\\ |:")])
    (<= (string->number a)
        (count (λ (x) (char=? (first (string->list c)) x)) (string->list e))
        (string->number b))))

(define (valid-password-2? str)
  (match-let ([(list a b c d e) (string-split str #px"\\-|\\ |:")])
    (xor (char=? (first (string->list c))
                 (list-ref (string->list e) (sub1 (string->number a))))
         (char=? (first (string->list c))
                 (list-ref (string->list e) (sub1 (string->number b)))))))

(display "test 1: ")
(~> test
    (string-split _ "\n")
    (map valid-password? _)
    (count identity _))

(display "one: ")
(~> data
    (string-split _ "\n")
    (map valid-password? _)
    (count identity _))

(display "test 2: ")
(~> test
    (string-split _ "\n")
    (map valid-password-2? _)
    (count identity _))

(display "two: ")
(~> data
    (string-split _ "\n")
    (map valid-password-2? _)
    (count identity _))