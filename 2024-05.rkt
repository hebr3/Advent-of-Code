#lang racket
(require threading)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2024-05.txt"))

(define test "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(define (string-split-blocks str)
  (map (λ (block) (string-split block "\n"))
       (string-split str "\n\n")))

(define (check-rule? rule update)
  ;(println (list rule update))
  (match-let ([(list A B) (map string->number (string-split rule "|"))])
    (if (and (member A update) (member B update))
        (< (index-of update A) (index-of update B))
        #t)))

(define (return-middle update)
  (list-ref update (floor (/ (length update) 2))))

(define (part-A L)
  (match-let ([(list RULES UPDATES) (string-split-blocks L)])
    (for/sum ([update UPDATES])
      (let ([UPDATES* (map string->number (string-split update ","))])
        (if (for/and ([rule RULES]) (check-rule? rule UPDATES*))
            (return-middle UPDATES*)
            0)))))

;(part-A test)
;(part-A data)

;;

; Function to build a graph as an adjacency list
(define (build-graph pairs)
  (define graph (make-hash))
  (define in-degrees (make-hash))
  
  (for ([pair pairs])
    (match-let ([(list a b) (string-split pair "|")])    
      ; Initialize graph nodes
      (hash-set! graph a (set-union (hash-ref graph a (set)) (set b)))
      (hash-set! graph b (hash-ref graph b (set)))
    
      ; Update in-degrees
      (hash-set! in-degrees a (hash-ref in-degrees a 0))
      (hash-set! in-degrees b (+ (hash-ref in-degrees b 0) 1))))
  
  (values graph in-degrees))

; Topological sort function
(define (topological-sort graph in-degrees)
  (define result '())
  (define zero-in-degree-nodes (filter (λ (node) (= (hash-ref in-degrees node 0) 0))
                                       (hash-keys graph)))
  
  (let loop ([queue zero-in-degree-nodes]
             [sorted result])
    (if (null? queue)
        (reverse sorted) ; Return the result in the correct order
        (begin
          (let* ([node (first queue)]
                 [neighbors (hash-ref graph node)])
          
            ; Decrease in-degree of neighbors
            (for ([neighbor (set->list neighbors)])
              (hash-set! in-degrees neighbor (- (hash-ref in-degrees neighbor 0) 1)))
          
            ; Add neighbors with zero in-degree to the queue
            (define new-queue (append (filter (λ (n) (= (hash-ref in-degrees n 0) 0))
                                              (set->list neighbors))
                                      (rest queue)))
            (loop new-queue (cons node sorted)))))))

; Main function
(define (get-ordered-list pairs)
  (define-values (graph in-degrees) (build-graph pairs))
  (topological-sort graph in-degrees))


(define (rule-in-set? rule update)
  (match-let ([(list A B) (map string->number (string-split rule "|"))])
    ;(println (list A B update))
    (and (member A update) (member B update))))

(define (part-B L)
  (match-let ([(list RULES UPDATES) (string-split-blocks L)])
    (define INCORRECT
      (filter list?
              (for/list ([update UPDATES])
                (let ([UPDATES* (map string->number (string-split update ","))])
                  (if (for/and ([rule RULES]) (check-rule? rule UPDATES*))
                      0
                      UPDATES*)))))
    (apply +
           (map return-middle
                (for/list ([incorrect INCORRECT])
                  (let ([RULES* (filter (λ (rule) (rule-in-set? rule incorrect)) RULES)])
                    (define ORDERED-PAIRS (map string->number (get-ordered-list RULES*)))
                    (filter (λ (x) (member x incorrect)) ORDERED-PAIRS)))))))

(part-B test)
(part-B data)
