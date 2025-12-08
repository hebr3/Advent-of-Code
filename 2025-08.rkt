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

(define (make-graph-from-pairs pairs)
  (define graph (make-hash))
  (for ([pair pairs])
    (match pair
      [(cons (list p1 p2) dist)
       (hash-update! graph p1 (λ (old) (cons p2 old)) '())
       (hash-update! graph p2 (λ (old) (cons p1 old)) '())]))
  graph)

(define (dfs graph start)
  (define visited (make-hash))
  (define (visit p)
    (unless (hash-ref visited p #f)
      (hash-set! visited p #t)
      (for ([n (hash-ref graph p '())])
        (visit n))))
  (visit start)
  (hash-keys visited))

(define (connected-components graph)
  (define seen (make-hash))
  (define comps '())
  (for ([p (hash-keys graph)])
    (unless (hash-ref seen p #f)
      (define comp (dfs graph p))
      (for ([q comp]) (hash-set! seen q #t))
      (set! comps (cons comp comps))))
  comps)

;; Main Function
(define (part-A input len)
  (define points (parse-input input))
  (define H (make-hash))
  (define (add! p1 p2)
    (define pair (if (string<? (format "~a" p1) (format "~a" p2))
                     (list p1 p2)
                     (list p2 p1)))
    (when (not (hash-has-key? H pair))
      (hash-set! H pair (distance p1 p2))))
  (for ([p1 points][i (in-naturals)])
    (for ([p2 (drop points (add1 i))])
      (add! p1 p2)))
  (define distances (hash->list H))
  (define graph (make-graph-from-pairs (take (sort distances < #:key cdr) len)))
  (define connected (connected-components graph))
  (apply * (take (sort (map length connected) >) 3)))

(check-equal? (part-A test 10) 40)

(part-A data 1000)

;;

(define (part-B input)
  input)

(check-equal? (part-B test) 25272)

;(part-B data)
