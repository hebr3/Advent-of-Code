#lang racket
(require threading
         "utils/helpers.rkt")

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

;; Test data
(define data (input->data "input/2024-16.txt"))
(define test "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
")
(define test2 "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
")

;; Structures
(struct Point [x y] #:transparent)
(struct pq [total_cost current prev_direction path] #:transparent)


;; Helper Functions
(define (build-graph input)
  (define lines (string-split input))
  (define graph (make-hash))

  (define (graph-append! a b)
    (hash-set! graph a (cons b (hash-ref graph a '()))))

  (define (path? a b)
    (and (member a (list #\. #\E #\S))
         (member a (list #\. #\E #\S))))
  
  (for ([y (sub1 (length lines))])
    (for ([x (string-length (first lines))])
      (let ([N (string-ref (list-ref lines y) x)]
            [S (string-ref (list-ref lines (add1 y)) x)])
        (when (path? N S)
          (graph-append! (Point x y) (Point x (add1 y)))
          (graph-append! (Point x (add1 y)) (Point x y))))))
  (for ([y (length lines)])
    (for ([x (sub1 (string-length (first lines)))])
      (let ([W (string-ref (list-ref lines y) x)]
            [E (string-ref (list-ref lines y) (add1 x))])
        (when (path? W E)
          (graph-append! (Point x y) (Point (add1 x) y))
          (graph-append! (Point (add1 x) y) (Point x y))))))
  
  graph)

(define (find-start-end input)
  (define lines (string-split input))
  (define start-point (Point 0 0))
  (define fake-previous (Point 0 0))
  (define end-point (Point 0 0))

  (for ([line lines][y (in-naturals)])
    (for ([ch line][x (in-naturals)])
      (match ch
        [#\. 'open]
        [#\S (set! start-point (Point x y))
             (set! fake-previous (Point (sub1 x) y))]
        [#\E (set! end-point (Point x y))]
        [#\# 'wall])))
  
  (list start-point fake-previous end-point))

(define (get-direction p1 p2)
  (match-let ([(Point x1 y1) p1]
              [(Point x2 y2) p2])
    (Point (- x2 x1) (- y2 y1))))

(define (calculate_cost prev_direction new_direction)
  (if (equal? prev_direction new_direction)
      1
      1001))

(define (part-A input)
  (define lines (string-split input))
  (define graph (build-graph input))
  (match-define (list start-point fake-previous end-point) (find-start-end input))

  (define priority-queue (mutable-set))
  (set-add! priority-queue (pq 0 start-point fake-previous (list start-point)))
  (define (pq-pop!)
    (let ([next (first (sort (set->list priority-queue) #:key pq-total_cost <))])
      (set-remove! priority-queue next)
      next))

  (define visited (mutable-set))

  (define (iter)
    (match-let ([(pq total_cost current prev_direction path) (pq-pop!)])
      (let ([state (list current prev_direction)])
        (cond
          [(equal? current end-point) (list path total_cost)]
          [(set-member? visited state) (iter)]
          [else
           (set-add! visited state)
           (for ([next_point (hash-ref graph current)])
             (define new_direction (get-direction current next_point))
             (define move_cost (calculate_cost prev_direction new_direction))
             (define new_cost (+ total_cost move_cost))
             (define new_path (cons next_point path))
             (set-add! priority-queue (pq new_cost next_point new_direction new_path)))
           (when (not (set-empty? priority-queue))
             (iter))]))))
  (match-define (list path cost) (iter))

  (define (draw-grid)
    (for ([line lines][y (in-naturals)])
      (for ([ch line][x (in-naturals)])
        (cond
          [(member (Point x y) path) (display #\O)]
          [else (display ch)]))
      (displayln "")))
  ;(draw-grid)
  
  cost)

        
(part-A test)
(part-A test2)
;(part-A data)

;;

(define (part-B input)
  (define lines (string-split input))
  (define graph (build-graph input))
  (match-define (list start-point fake-previous end-point) (find-start-end input))

  (define priority-queue (mutable-set))
  (set-add! priority-queue (pq 0 start-point fake-previous (list start-point)))
  (define (pq-pop!)
    (let ([next (first (sort (set->list priority-queue) #:key pq-total_cost <))])
      (set-remove! priority-queue next)
      next))

  (define visited (make-hash))
  (define optimal_paths (mutable-set))
  (define min_cost +inf.0)

  (define (iter)
    (match-let ([(pq total_cost current prev_direction path) (pq-pop!)])
      (let ([state (list current prev_direction)])
        (cond
          [(and (not (set-empty? optimal_paths)) (> total_cost min_cost))
           (list optimal_paths min_cost)]
          [(equal? current end-point)
           (cond
             [(or (set-empty? optimal_paths) (= total_cost min_cost))
              (set-add! optimal_paths path)
              (set! min_cost total_cost)
              (iter)]
             [else 'longer])]
          [(and (hash-has-key? visited state) (< (hash-ref visited state) total_cost))
           (iter)]
          [else
           (hash-set! visited state total_cost)
           (for ([next_point (hash-ref graph current)])
             (define new_direction (get-direction current next_point))
             (define move_cost (calculate_cost prev_direction new_direction))
             (define new_cost (+ total_cost move_cost))
             (when (or (set-empty? optimal_paths) (<= new_cost min_cost))
               (define new_path (cons next_point path))
               (set-add! priority-queue (pq new_cost next_point new_direction new_path))))
           (when (not (set-empty? priority-queue))
             (iter))]))))
  
  (match-define (list paths cost) (iter))

  (define (draw-grid)
    (for ([line lines][y (in-naturals)])
      (for ([ch line][x (in-naturals)])
        (cond
          [(member (Point x y) (flatten (set->list paths))) (display #\O)]
          [else (display ch)]))
      (displayln "")))
  (draw-grid)
  
  (set-count (list->set (flatten (set->list paths)))))

;(part-B test)
(part-B test2)
(part-B data)