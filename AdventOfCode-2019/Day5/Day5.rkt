#lang racket
(require rackunit)

(define input
  (file->lines "input.txt"))

(define (day5 in arr)
  (define (iter idx arr*)
    (let ([opt (list-ref arr* idx)])
      (cond
        [(= 99 opt)
         arr*]
        [else
         (let* ([instruction (list-ref arr* idx)]
                [opt (modulo instruction 100)]
                [first-param (modulo (quotient instruction 100) 10)]
                [second-param (modulo (quotient instruction 1000) 10)]
                [third-param (modulo (quotient instruction 10000) 10)])
           (let ([a (list-ref arr* (list-ref arr* (+ 1 idx)))]
                 [b (list-ref arr* (list-ref arr* (+ 2 idx)))]
                 [c (list-ref arr* (+ 3 idx))])
             (let ([calc (if (= 1 opt) (+ a b) (* a b))])
             (iter (+ 4 idx) (list-set arr* c calc)))))])))
  (iter 0 arr))

(check-equal? (day5 1 '(1 0 0 0 99)) '(2 0 0 0 99) "test1")
(check-equal? (day5 1 '(2 3 0 3 99)) '(2 3 0 6 99) "test2")
(check-equal? (day5 1 '(2 4 4 5 99 0)) '(2 4 4 5 99 9801) "test3")
(check-equal? (day5 1 '(1 1 1 4 99 5 6 0 99)) '(30 1 1 4 2 5 6 0 99) "test4")
(check-equal? (day5 1 '(3 0 4 0 99)) 1 "test5")

;(define (step a b)
;  (car
;   (let ([arr (map string->number (string-split (car input) ","))])
;     (day5 (list-set (list-set arr 1 a) 2 b)))))

;(for* ([a 100][b 100])
;  (when (= 19690720 (step a b))
;    (displayln (+ (* 100 a) b))))