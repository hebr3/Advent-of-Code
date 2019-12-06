#lang racket

(define input
  (file->lines "input.txt"))

(define (day2 arr)
  (define (iter idx arr*)
    (if (= 99 (list-ref arr* idx))
        arr*
        (let ([opt (list-ref arr* idx)]
              [a (list-ref arr* (list-ref arr* (+ 1 idx)))]
              [b (list-ref arr* (list-ref arr* (+ 2 idx)))]
              [c (list-ref arr* (+ 3 idx))])
          (let ([calc (if (= 1 opt) (+ a b) (* a b))])
            (iter (+ 4 idx) (list-set arr* c calc))))))
  (iter 0 arr))

(day2 '(1 0 0 0 99))
(day2 '(2 3 0 3 99))
(day2 '(2 4 4 5 99 0))
(day2 '(1 1 1 4 99 5 6 0 99))

(define (step a b)
  (car
   (let ([arr (map string->number (string-split (car input) ","))])
     (day2 (list-set (list-set arr 1 a) 2 b)))))

(for* ([a 100][b 100])
  (when (= 19690720 (step a b))
    (displayln (+ (* 100 a) b))))