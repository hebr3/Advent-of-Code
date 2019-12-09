#lang racket

(define input
  (file->lines "input.txt"))

(define test
  '("123456789012"))

(define (input->List-of-Numbers in)
  (map (λ (c) (string->number (string c)))
       (string->list (car in))))

(input->List-of-Numbers test)

(define (LoN->Picture lon width height)
  (let* ([len (length lon)]
         [layers (/ len (* width height))])
    (for/list ([h height])
      (for*/list ([l layers][w width])
        (list-ref lon (+ w (* width h) (* l width height)))))))

(LoN->Picture (input->List-of-Numbers test) 3 2)

(define Picture
  (LoN->Picture (input->List-of-Numbers input) 25 6))

(map (λ (x) (count zero? x)) Picture)

(define (one? x) (= 1 x))
(define (two? x) (= 2 x))

(map (λ (x) (count one? x)) Picture)
(map (λ (x) (count two? x)) Picture)