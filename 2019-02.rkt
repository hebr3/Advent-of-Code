#lang racket
(require rackunit)
(require threading)
(require racket/match)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define input (input->data "input/2019-02.txt"))

(define test1 "1,9,10,3,2,3,11,0,99,30,40,50
")
(define test2 "1,0,0,0,99
")
(define test3 "2,3,0,3,99
")
(define test4 "2,4,4,5,99,0
")
(define test5 "1,1,1,4,99,5,6,0,99
")

(define (add idx vec)
  (match-let ([(vector a b c d ...) (vector-drop vec (add1 idx))])
    (let ([A (vector-ref vec a)]
          [B (vector-ref vec b)]
          [C (vector-ref vec c)])
      (println (list vec a b c A B C))
      (vector-set! vec C (+ A B)))))

(define (mult idx vec)
  (match-let ([(vector a b c d ...) (vector-drop vec (add1 idx))])
    (let ([A (vector-ref vec a)]
          [B (vector-ref vec b)]
          [C (vector-ref vec c)])
      (println (list vec a b c A B C))
      (vector-set! vec C (* A B)))))

(define (part-A L)
  (let* ([los (string-split (first (string-split L "\n")) ",")]
         [lon (map string->number los)]
         [vec (list->vector lon)])
    (define (iter idx)
      (println vec)
      (cond
        [(= (vector-ref vec idx) 99) vec]
        [(= (vector-ref vec idx) 1)
         (add idx vec)
         (iter (+ 4 idx))]
        [(= (vector-ref vec idx) 2)
         (mult idx vec)
         (iter (+ 4 idx))]))
    (iter 0)))

;(part-A input)

(part-A test1)
(part-A test2)
(part-A test3)
(part-A test4)
(part-A test5)
;; (day2 '(2 3 0 3 99))
;; (day2 '(2 4 4 5 99 0))
;; (day2 '(1 1 1 4 99 5 6 0 99))
;; 
;; (check-equal? (day2 '(1 0 0 0 99)) '(2 0 0 0 99) "test1")
;; (check-equal? (day2 '(2 3 0 3 99)) '(2 3 0 6 99) "test2")
;; (check-equal? (day2 '(2 4 4 5 99 0)) '(2 4 4 5 99 9801) "test3")
;; (check-equal? (day2 '(1 1 1 4 99 5 6 0 99)) '(30 1 1 4 2 5 6 0 99) "test4")
;; 
;; 
;; (define (step a b)
;;   (car
;;    (let ([arr (map string->number (string-split input ","))])
;;      (day2 (list-set (list-set arr 1 a) 2 b)))))
;; 
;; (for* ([a 100][b 100])
;;   (when (= 19690720 (step a b))
;;     (displayln (+ (* 100 a) b))))
