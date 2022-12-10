#lang racket
(require threading)
(require racket/match)
(require rackunit)

(define data "0,1,4,13,15,12,16")
(define test "0,3,6")

;;---

(define (trunc-to-2 L)
  (if (< 2 (length L))
      (take L 2)
      L))

(define (run MAX L)
  (define HT (make-hash))
  (for ([i L])
    (hash-set! HT i (list (add1 (index-of L i)))))
  (define (iter COUNT LAST)
    (cond
      [(<= MAX COUNT) LAST]
      [(<= 2 (length (hash-ref HT LAST)))
       (let ([a (first (hash-ref HT LAST))]
             [b (second (hash-ref HT LAST))])
         (hash-set! HT (- a b) (trunc-to-2 (cons (add1 COUNT) (hash-ref HT (- a b) '()))))
         (iter (add1 COUNT) (- a b)))]
      [(= 1 (length (hash-ref HT LAST)))
       (hash-set! HT 0 (trunc-to-2 (cons (add1 COUNT) (hash-ref HT 0 '()))))
       (iter (add1 COUNT) 0)]
      [else
       (iter (add1 COUNT) LAST)]))
  (iter (length L) (last L)))

(define (part-A L)
  (~>> L
       (string-split _ ",")
       (map string->number)
       (run 2020)))

(part-A test)
(part-A data)

;;---

(define (part-B L lim)
  (~>> L
       (string-split _ ",")
       (map string->number)
       (run lim)))

(time (part-B test 3000))
(time (part-B test 30000))
(time (part-B test 300000))
(time (part-B test 3000000))
(time (part-B test 30000000))

(time (part-B "0,3,6" 30000000))
(time (part-B "1,3,2" 30000000))
(time (part-B "2,1,3" 30000000))
(time (part-B "1,2,3" 30000000))
(time (part-B "2,3,1" 30000000))
(time (part-B "3,2,1" 30000000))
(time (part-B "3,1,2" 30000000))

(time (part-B data 30000000))