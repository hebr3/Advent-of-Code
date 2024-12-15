#lang racket
(require "utils/matrix.rkt")
;(require "utils/point-2d.rkt")
(require "utils/input.rkt")

(define data (input->data "input/2023-25.txt"))

(define test "jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr
")

;;

(define (random-element lst)
  (list-ref lst (random (length lst))))

(define (merge-nodes graph node1 node2)
  (let ([merged-edges (append (hash-ref graph node1) (hash-ref graph node2))])
    (hash-remove! graph node2)
    (hash-set! graph node1 (remove node2 (remove node1 merged-edges)))
    (for ([edge (remove node2 (remove node1 merged-edges))])
      (let ([old-edges (hash-ref graph edge)])
        (when (not (member node1 old-edges))
          (hash-update! graph edge (λ (old) (cons node1 old)) '()))))
    graph))

(define (find-min-cut graph)
  (define (iter current-graph)
    (let ([graph-list (hash->list current-graph)])
      (if (= 2 (length graph-list))
        (length (cdr (first graph-list)))
        (let* ([node1 (random-element (hash-keys current-graph))]
               [edges1 (hash-ref current-graph node1)]
               [node2 (random-element edges1)])
          (iter (merge-nodes current-graph node1 node2))))))
  (iter graph))
  

(define (part-A L)
  (let ([rows (string-split L "\n")]
        [G (make-hash)])
    (for ([row rows])
      (let* ([parts (string-split row #px"(: )|( )")]
             [head (first parts)]
             [tail (rest parts)])
        (for ([t tail])
          (hash-update! G head (λ (old) (cons t old)) '())
          (hash-update! G t    (λ (old) (cons head old)) '()))))
    (find-min-cut G)
    ))

(part-A test)
;(part-A data)

;;

(define (part-B L)
  L)

;(part-B test)
;(part-B data)

