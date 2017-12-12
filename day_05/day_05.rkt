#lang racket
(require threading)

(define t1 (list 0 3 0 1 -3))

(define in
  (~> "input.txt"
      file->list))

(define (step lst n count)
  (when (zero? (modulo count 1000)) (println count))
  (if (> n (sub1 (length lst)))
      count
      (let* ([i (list-ref lst n)]
             [lst2 (list-update lst n add1)])
        (step lst2 (+ n i) (add1 count)))))

(step t1 0 0)
;(step in 0 0)

(define (step2 lst n count)
  (when (zero? (modulo count 1000000)) (println count))
  (if (or (< n 0) (> n (sub1 (length lst))))
      (println count)
      (let* ([i (list-ref lst n)]
             [lst2 (list-update lst n (if (<= 3 i) sub1 add1))])
        (step2 lst2 (+ n i) (add1 count)))))

(step2 t1 0 0)
(step2 in 0 0)

(step2 '(0 1 0 -5) 0 0)