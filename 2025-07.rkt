#lang racket

(require rackunit)
(require racket/set)

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

;; Test data
(define data (input->data "input/2025-07.txt"))

(define test ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

;; Helper Function
(struct point [x y] #:transparent)
(struct board [start splitters length] #:transparent)

(define (point-below? pt S)
  (match-let ([(point x y) pt])
    (set-member? S (point x (add1 y)))))

(define (point-above? pt S)
  (match-let ([(point x y) pt])
    (set-member? S (point x (sub1 y)))))

(define (point-down! pt S)
  (match-let ([(point x y) pt])
    (set-add! S (point x (add1 y)))))
(define (point-split! pt S)
  (match-let ([(point x y) pt])
    (set-add! S (point (sub1 x) (add1 y)))
    (set-add! S (point (add1 x) (add1 y)))))

(define (input->board input)
  (define lines (string-split input "\n"))
  (let ([start (point 0 0)]
        [splitters (mutable-set)])
    (for ([line lines][row (in-naturals)])
      (for ([ch line][col (in-naturals)])
        (when (char=? #\S ch)
          (set! start (point col row)))
        (when (char=? #\^ ch)
          (set-add! splitters (point col row)))))
    (board start splitters (length lines))))
  

;; Main Function
(define (part-A input)
  (match-define (board start splitters board-length) (input->board input))
  (define beams (mutable-set))
  (define (tick! count)
    (define current (filter (λ (pt) (= count (point-y pt))) (set->list beams)))
    (cond
      [(= board-length count) beams]
      [else
       (for ([pt current])
         (if (point-below? pt splitters)
             (point-split! pt beams)
             (point-down! pt beams)))
       (tick! (add1 count))]))
  (point-down! start beams)
  (tick! 1)
  (for/sum ([splitter splitters])
    (if (point-above? splitter beams) 1 0)))

(check-equal? (part-A test) 21)

(part-A data)

;;

(define (part-B input)
  (match-define (board start splitters board-length) (input->board input))
  (define lookup (make-hash))
  (define (solve pt)
    (hash-ref! lookup pt (λ () (solve! pt))))
  (define (solve! pt)
    (match-define (point x y) pt)
    (define down (point x (add1 y)))
    (cond
      [(>= y board-length)
       (hash-set! lookup pt 1)
        1]
      [(set-member? splitters down)
       (define left (solve (point (sub1 x) (add1 y))))
       (define right (solve (point (add1 x) (add1 y))))
       (+ left right)]
      [else
       (solve down)]))
  (solve start))

(check-equal? (part-B test) 40)

(part-B data)
