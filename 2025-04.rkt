#lang racket

(require rackunit)
(require racket/set)

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

;; Test data
(define data (input->data "input/2025-04.txt"))

(define test "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

;; Helper Function
(struct point [x y] #:transparent)

(define (string->graph str)
  (define H (mutable-set))
  (for ([line (string-split str "\n")][row (in-naturals)])
    (for ([ch line][col (in-naturals)])
      (when (char=? #\@ ch)
        (set-add! H (point col row)))))
  H)

(define (check-graph G)
  (define keys (set->list G))
  (define (neighbor-count k)
    (match-let ([(point x y) k])
      (let ([N  (point (+ x 0) (- y 1))]
            [NE (point (+ x 1) (- y 1))]
            [E  (point (+ x 1) (+ y 0))]
            [SE (point (+ x 1) (+ y 1))]
            [S  (point (+ x 0) (+ y 1))]
            [SW (point (- x 1) (+ y 1))]
            [W  (point (- x 1) (+ y 0))]
            [NW (point (- x 1) (- y 1))])
        (for/sum ([p (list N NE E SE S SW W NW)])
          (if (set-member? G p) 1 0)))))
  (define H (make-hash))
  (for ([key keys])
    (when (< (neighbor-count key) 4)
      (hash-set! H key #t)))
  H)

(define (print-graph G)
  (define keys (set->list G))
  (define max-x (apply max (map point-x keys)))
  (define max-y (apply max (map point-y keys)))
  (for ([y (add1 max-y)])
    (for ([x (add1 max-x)])
      (if (set-member? G (point x y))
          (display #\@)
          (display #\.)))
    (displayln ""))
  (displayln ""))

;; Main Function
(define (part-A input)
  (define G (string->graph input))
  ;(print-graph G)
  (define less-4 (check-graph G))
  ;(print-graph less-4)
  (length (hash-keys less-4)))

(check-equal? (part-A test) 13)

(part-A data)

;;

(define (part-B input)
  (define G (string->graph input))
  (define init-rolls (set->list G))
  (define (iter g)
    (define open (check-graph g))
    (define open-keys (hash-keys open))
    (cond
      [(empty? open-keys) g]
      [else
       (for ([key open-keys])
         (set-remove! g key))
       ;(print-graph g)
       (iter g)]))
  (iter G)
  (define final-rolls (set->list G))
  (- (length init-rolls)
     (length final-rolls)))

(check-equal? (part-B test) 43)

(part-B data)
