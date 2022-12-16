#lang racket
(require threading)
(require graph)

(define data
  (~> "input/2022-16.txt"
      open-input-file
      (read-line _ 'return)
      ))

(define test "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")

;;---

(struct Valve [label rate leads] #:transparent)
(struct State [loc time] #:transparent)

(define (make-valve str)
  (match-let ([(list label _ rate _ leads) (string-split str #px"Valve | has|=|;|valves |valve ")])
    (Valve label (string->number rate) (string-split leads ", "))))

(define (fact n)
  (define (iter acc c)
    (if (zero? c)
        acc
        (iter (* acc c) (sub1 c))))
  (iter 1 n))

(define (part-A in)
  (define input (string-split in "\n"))
  (define valves (map make-valve input))

  ;; Initialize Graph
  (define g (unweighted-graph/undirected
             (for*/list ([t valves][lead (Valve-leads t)])
               (list (Valve-label t) lead))))

  ;(displayln (graphviz g))

  (define (calculate-list LoV)
    ;; Current State
    (define Open-HT (make-hash))
    (for ([v valves])
      (hash-set! Open-HT (Valve-label v) 0))
  
    ;; State
    (define Current-Time 30)
    (define Current-Loc "AA")

    (define (move-and-open loc)
      (define start Current-Loc)
      (let-values ([(dist-ht adj) (dijkstra g start)])
        (define dist (hash-ref dist-ht loc))
        (when (< (add1 dist) Current-Time)
          (set! Current-Time (- Current-Time dist 1))
          (set! Current-Loc loc)
          (hash-set! Open-HT loc Current-Time))))

    (for ([v LoV])
      (move-and-open v))
    
    (let ([SUM (for/sum ([v valves])
                 (* (Valve-rate v) (hash-ref Open-HT (Valve-label v))))])
      ;(displayln SUM)
      SUM))

  (define good-valves
    (~>> valves
         (filter (λ (v) (not (zero? (Valve-rate v)))))
         (sort _ (λ (a b) (> (Valve-rate a) (Valve-rate b))))
         (map Valve-label)
         ))
  ;(println good-valves)
  
  (~>> (for/list ([p (stream-take (sequence->stream (in-permutations good-valves))
                                  (floor (/ (fact (length good-valves)) 2)))])
         (calculate-list p))
       (apply max))
  )

(part-A test)
;(part-A data)

