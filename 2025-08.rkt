#lang racket

(require rackunit)
(require racket/set)

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))
    #:mode 'text))

;; Test data
(define data (input->data "input/2025-08.txt"))

(define test "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")

;; Helper Function
(struct point [x y z] #:transparent)
(struct edge [p1 p2 dist] #:transparent)

(define (distance p1 p2)
  (match-define (point x1 y1 z1) p1)
  (match-define (point x2 y2 z2) p2)
  (sqrt
   (+ (sqr (- x1 x2))
      (sqr (- y1 y2))
      (sqr (- z1 z2)))))

(define (parse-input input)
  (define lines (string-split input "\n"))
  (for/list ([line lines])
    (match-define (list x y z)
      (map string->number (string-split line ",")))
    (point x y z)))

(define (all-point-pairs pts)
  (combinations pts 2))

(define (all-point-pairs-with-distance pts)
  (for*/list ([i (in-range (length pts))]
              [j (in-range (+ i 1) (length pts))])
    (define p1 (list-ref pts i))
    (define p2 (list-ref pts j))
    (edge p1 p2 (distance p1 p2))))

(define (sort-edges edges)
  (sort edges < #:key edge-dist))

(define (sort-pairs-by-distance pairs)
  (sort pairs
        (λ (a b)
          (< (distance (first a) (second a))
             (distance (first b) (second b))))))

(define (make-graph-from-pairs pairs)
  (define graph (make-hash))
  (for ([pair pairs])
    (match pair
      [(list p1 p2)
       (hash-update! graph p1 (λ (old) (set-add old p2)) (set))
       (hash-update! graph p2 (λ (old) (set-add old p1)) '())]))
  graph)

(define (dfs graph start)
  (define visited (mutable-set))
  (define (visit p)
    (unless (set-member? visited p)
      (set-add! visited p)
      (for ([n (hash-ref graph p (set))])
        (visit n))))
  (visit start)
  (set->list visited))

(define (connected-components graph)
  (define seen (make-hash))
  (define comps '())
  (for ([p (hash-keys graph)])
    (unless (hash-ref seen p #f)
      (define comp (dfs graph p))
      (for ([q comp]) (hash-set! seen q #t))
      (set! comps (cons comp comps))))
  comps)

(define (add-edge! g p1 p2)
  (hash-update! g p1 (λ (old) (cons p2 old)) '())
  (hash-update! g p2 (λ (old) (cons p1 old)) '()))

(define (connected? g)
  (define nodes (hash-keys g))
  (cond
    [(empty? nodes) #t]
    [else
     (= (length (dfs g (first nodes)))
        (length nodes))]))

(define (make-empty-graph points)
  (define g (make-hash))
  (for ([p points])
    (hash-set! g p '()))
  g)

(define (first-edge-that-connects points edges)
  (define g (make-empty-graph points))

  (let ([stop #f])
    (for ([pair edges] #:break (connected? g))
      (match pair
        [(list p1 p2)
         (add-edge! g p1 p2)
         (when (connected? g)
           (set! stop (list p1 p2)))]))
    stop))

;; Main Function
(define (part-A input len)
  (define points (parse-input input))
  (define pairs (all-point-pairs points))
  (define sorted-pairs (sort-pairs-by-distance pairs))
  (define graph (make-graph-from-pairs (take sorted-pairs len)))
  (define connected (connected-components graph))
  (apply * (take (sort (map length connected) >) 3)))

(check-equal? (part-A test 10) 40)

(part-A data 1000)

;;

(define (part-B input)
  (define points (parse-input input))
  (define pairs (all-point-pairs points))
  (define sorted-pairs (sort-pairs-by-distance pairs))
  (define final (first-edge-that-connects points sorted-pairs))
  (* (point-x (first final))
     (point-x (second final))))

(check-equal? (part-B test) 25272)

(part-B data)
