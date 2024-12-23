#lang racket
(require data/queue)

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

;; Test data
(define data (input->data "input/2024-23.txt"))

(define test "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
")

;; Structs

;; Helper

;; Main Function

(define (part-A input)
  (define lines (string-split input "\n"))
  (define graph (make-hash))
  (define (graph-add! a b)
    (hash-set! graph a (cons b (hash-ref graph a '()))))
  (for ([line lines])
    (match-define (list a b) (string-split line "-"))
    (graph-add! a b)
    (graph-add! b a))
  (define three-set (mutable-set))
  (for ([k1 (hash-keys graph)] #:when (string-prefix? k1 "t"))
    (define v1 (hash-ref graph k1))
    (for ([k2 v1])
      (define v2 (hash-ref graph k2))
      (for ([v v2])
        (when (member v v1)
          (set-add! three-set (sort (list k1 k2 v) string<?))))))
  (set-count three-set))

(part-A test)
(part-A data)
  
;;

(struct graph (adjacency-list) #:transparent)

; Create an empty graph
(define (make-graph)
  (graph (make-hash)))

(define (add-edge! g v1 v2)
  (define adj-list (graph-adjacency-list g))
  (hash-set! adj-list v1 (set-add (hash-ref adj-list v1 (set)) v2))
  (hash-set! adj-list v2 (set-add (hash-ref adj-list v2 (set)) v1)))

; Get neighbors of a vertex
(define (get-neighbors g v)
  (hash-ref (graph-adjacency-list g) v (set)))

; Find vertex with maximum degree in a set of vertices
(define (find-pivot g vertices)
  (define (vertex-count v)
    (set-count (get-neighbors g v)))
  (cond
    [(set-empty? vertices) #f]
    [else
     (argmax vertex-count (set->list vertices))]))

; Main Bron-Kerbosch recursive procedure
(define (bron-kerbosch g r p x)
  (cond
    [(and (set-empty? p) (set-empty? x))
     ; Found a maximal clique
     r]
    [else
     (define pivot (find-pivot g (set-union p x)))
     (define vertices-to-process
       (if pivot (set-subtract p (get-neighbors g pivot)) p))
     (for/fold ([max-clique (set)])
               ([v (in-set vertices-to-process)])
       (let* ([neighbors (get-neighbors g v)]
              [new-r (set-add r v)]
              [new-p (set-intersect p neighbors)]
              [new-x (set-intersect x neighbors)]
              [clique (bron-kerbosch g new-r new-p new-x)])
           ; Keep the larger clique
         (if (> (set-count clique) (set-count max-clique))
             clique
             max-clique)))]))

; Main function to find maximum clique
(define (find-maximum-clique g)
  (let ([vertices (list->set (hash-keys (graph-adjacency-list g)))])
    (bron-kerbosch g (set) vertices (set))))

(define (part-B input)
  (define lines (string-split input "\n"))
  (define g (make-graph))
  (for ([line lines])
    (match-define (list a b) (string-split line "-"))
    (add-edge! g a b))
  (string-join (sort (set->list (find-maximum-clique g)) string<?) ","))

(part-B test)
(part-B data)
