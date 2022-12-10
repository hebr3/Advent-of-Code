#lang racket
(require threading)
(require racket/match)
(require rackunit)

(define data "0,1,4,13,15,12,16")
(define test "0,3,6")

;;---

(define (run L)
  (cond
    [(< 2020 (length L)) L]
    [(zero? (count (λ (x) (= x (first L))) (rest L))) (run (cons 0 L))]
    [else
     (run (cons (add1 (index-of (rest L) (first L))) L))]))

(define (part-A L)
  (~>> L
       (string-split _ ",")
       (map string->number)
       reverse
       run
       cadr))

(part-A test)
(part-A data)

;;---

(define (run2 L)
  (cond
    [(< 30000000 (length L)) L]
    [(zero? (count (λ (x) (= x (first L))) (rest L))) (run2 (cons 0 L))]
    [else
     (run2 (cons (add1 (index-of (rest L) (first L))) L))]))

(define (part-B L)
  (~>> L
       (string-split _ ",")
       (map string->number)
       reverse
       run2
       cadr))

(part-B test)
(part-B data)