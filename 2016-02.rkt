#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2016-02.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")
      ))

(define test '("ULL"
               "RRDDD"
               "LURDL"
               "UUUUD"))

;; helper

(define (string->list-of-string str)
  (map string (string->list str)))

(define (number-or-string->string c)
  (if (number? c)
      (number->string c)
      c))
;; functions
(define (move pos dir)
  (cond
    [(string=? "U" dir)
     (cond
       [(= pos 4) 1]
       [(= pos 5) 2]
       [(= pos 6) 3]
       [(= pos 7) 4]
       [(= pos 8) 5]
       [(= pos 9) 6]
       [else pos])]
    [(string=? "D" dir)
     (cond
       [(= pos 1) 4]
       [(= pos 2) 5]
       [(= pos 3) 6]
       [(= pos 4) 7]
       [(= pos 5) 8]
       [(= pos 6) 9]
       [else pos])]
    [(string=? "R" dir)
     (cond
       [(= pos 1) 2]
       [(= pos 4) 5]
       [(= pos 7) 8]
       [(= pos 2) 3]
       [(= pos 5) 6]
       [(= pos 8) 9]
       [else pos])]
    [(string=? "L" dir)
     (cond
       [(= pos 3) 2]
       [(= pos 6) 5]
       [(= pos 9) 8]
       [(= pos 2) 1]
       [(= pos 5) 4]
       [(= pos 8) 7]
       [else pos])]))

(define (move-iter start LoS)
  (if (empty? LoS)
      start
      (move-iter (move start (first LoS)) (rest LoS))))

(define (move-iter-iter acc LoLoS)
  (cond
    [(empty? LoLoS)
     acc]
    [else
     (move-iter-iter (cons (move-iter (first acc) (first LoLoS)) acc) (rest LoLoS))]))

(define (part-A L)
  (~>> L
       (map string->list-of-string)
       (move-iter-iter '(5))
       reverse
       cdr
       (map number->string)
       (string-join _ "")))

(part-A test)
(part-A data)

;;---
(define (move2 pos dir)
  (cond
    [(string=? "U" dir)
     (cond
       [(eq? pos 3) 1]
       [(eq? pos 6) 2]
       [(eq? pos 7) 3]
       [(eq? pos 8) 4]
       [(eq? pos "A") 6]
       [(eq? pos "B") 7]
       [(eq? pos "C") 8]
       [(eq? pos "D") "B"]
       [else pos])]
    [(string=? "D" dir)
     (cond
       [(eq? pos 1) 3]
       [(eq? pos 2) 6]
       [(eq? pos 3) 7]
       [(eq? pos 4) 8]
       [(eq? pos 6) "A"]
       [(eq? pos 7) "B"]
       [(eq? pos 8) "C"]
       [(eq? pos "B") "D"]
       [else pos])]
    [(string=? "R" dir)
     (cond
       [(eq? pos 2) 3]
       [(eq? pos 3) 4]
       [(eq? pos 5) 6]
       [(eq? pos 6) 7]
       [(eq? pos 7) 8]
       [(eq? pos 8) 9]
       [(eq? pos "A") "B"]
       [(eq? pos "B") "C"]
       [else pos])]
    [(string=? "L" dir)
     (cond
       [(eq? pos 3) 2]
       [(eq? pos 4) 3]
       [(eq? pos 6) 5]
       [(eq? pos 7) 6]
       [(eq? pos 8) 7]
       [(eq? pos 9) 8]
       [(eq? pos "B") "A"]
       [(eq? pos "C") "B"]
       [else pos])]))

(define (move-iter2 start LoS)
  (if (empty? LoS)
      start
      (move-iter2 (move2 start (first LoS)) (rest LoS))))

(define (move-iter-iter2 acc LoLoS)
  (cond
    [(empty? LoLoS)
     acc]
    [else
     (move-iter-iter2 (cons (move-iter2 (first acc) (first LoLoS)) acc) (rest LoLoS))]))

(define (part-B L)
  (~>> L
       (map string->list-of-string)
       (move-iter-iter2 '(5))
       reverse
       cdr
       (map number-or-string->string)
       (string-join _ "")))

(part-B test)
(part-B data)
