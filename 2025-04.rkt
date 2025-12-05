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
  (for ([line (string-split str "\n")]
        [row (in-naturals)])
    (for ([ch line]
          [col (in-naturals)])
      (when (char=? #\@ ch)
        (set-add! H (point col row)))))
  H)

(define (neighbor-count G k)
  (match-define (point x y) k)
  (for*/sum ([i '(-1 0 1)]
             [j '(-1 0 1)]
             #:when (not (= 0 i j)))
    (if (set-member? G (point (+ x i) (+ y j))) 1 0)))

(define (find-points-with-neighbors-less G val)
  (define keys (set->list G))
  (define H (mutable-set))
  (for ([key keys])
    (when (< (neighbor-count G key) val)
      (set-add! H key)))
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

(define (shrink-graph! g)
  (define open (find-points-with-neighbors-less g 4))
  (define open-keys (set->list open))
  (cond
    [(empty? open-keys) g]
    [else
     (for ([key open-keys])
       (set-remove! g key))
     ;(print-graph g)
     (shrink-graph! g)]))

;; Main Function
(define (part-A input)
  (define G (string->graph input))
  ;(print-graph G)
  (define less-4 (find-points-with-neighbors-less G 4))
  ;(print-graph less-4)
  (length (set->list less-4)))

(check-equal? (part-A test) 13)

(part-A data)

;;

(define (part-B input)
  (define G (string->graph input))
  (define init-rolls (set->list G))
  (shrink-graph! G)
  (define final-rolls (set->list G))
  (- (length init-rolls)
     (length final-rolls)))

(check-equal? (part-B test) 43)

(part-B data)
