#lang racket
(require threading)

(define (char->int c)
  (string->number (string c)))

(let* ([in (file->list "input.txt" #:mode 'text)]
       [num (number->string (car in))]
       [nums (string->list num)]
       [num-list (map char->int nums)]
       [n (length num-list)]
       [fst (list (car num-list))]
       [rst (rest num-list)]
       [num-list2 (append rst fst)]
       [one (for/sum ([i num-list][j num-list2])
              (if (= i j) i 0))]
       [fst2 (take num-list (/ n 2))]
       [rst2 (drop num-list (/ n 2))]
       [num-list3 (append rst2 fst2)]
       [two (for/sum ([i num-list][j num-list3])
              (if (= i j) i 0))])
  (list one two))

;(~> "input.txt"
;    file->list
;    car
;    number->string
;    string->list
;    (map char->int _))