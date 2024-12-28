#lang racket

(require data/heap)

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

;; Test data
(define data (input->data "input/2024-18.txt"))

(define test "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
")


;; Structure definitions with contracts
(struct node [x y t] #:transparent)

(struct planner [width height max-time] #:transparent)

;; Helper functions with contracts
(define (valid-position? p n)
  ;(-> planner? node? boolean?)
  (match-define (node x y t) n)
  (match-define (planner width height max-time) p)
  (and (<= 0 x (sub1 width))
       (<= 0 y (sub1 height))
       (< t max-time)))

(define (manhattan-distance n1 n2)
  ;(-> node? node? exact-nonnegative-integer?)
  (match-define (node n1x n1y _) n1)
  (match-define (node n2x n2y _) n2)
  (+ (abs (- n1x n2x))
     (abs (- n1y n2y))))

(define (heuristic current goal)
  ;(-> node? node? exact-nonnegative-integer?)
  (max (manhattan-distance current goal)
       (abs (- (node-t current) (node-t goal)))))

(define (get-neighbors p n)
  ;(-> planner? node? (listof node?))
  (define moves '((0 1) (0 -1) (1 0) (-1 0)))
  (match-define (node x y t) n)
  (filter (λ (new-n) (valid-position? p new-n))
          (for/list ([move moves])
            (match-let ([(list dx dy) move])
              (node (+ x dx)
                    (+ y dy)
                    (add1 t))))))

(define (valid-move? n obstacles)
  ;(-> node? obstacles? boolean?)
  (match-define (node x y t) n)
  (not (set-member? obstacles (list x y t))))

(define (reconstruct-path came-from current)
  ;(-> (hash/c node? node? #:immutable #f) node? path?)
  (define (iter curr acc)
    (cond
      [(not (hash-has-key? came-from curr))
       (cons curr acc)]
      [else (iter (hash-ref came-from curr) (cons curr acc))]))
  (iter current '()))

;; Priority Queue operations
(define-values (make-queue enqueue! dequeue! queue-empty?)
  (let ()
    (define (make-queue)
      (make-heap (λ (a b) (< (car a) (car b)))))
    
    (define (enqueue! queue priority item)
      (heap-add! queue (cons priority item)))
    
    (define (dequeue! queue)
      (define pair (heap-min queue))
      (heap-remove-min! queue)
      (cdr pair))
    
    (define (queue-empty? queue)
      (zero? (heap-count queue)))
    
    (values make-queue enqueue! dequeue! queue-empty?)))

;; Main algorithm implementation
(define (find-path p start goal obstacles)
  (define open-queue (make-queue))
  (define closed-set (mutable-set))
  (define g-scores (make-hash))
  (define f-scores (make-hash))
  (define came-from (make-hash))
  
  ;; Initialize starting node
  (hash-set! g-scores start 0)
  (hash-set! f-scores start (heuristic start goal))
  (enqueue! open-queue (hash-ref f-scores start) start)
  
  (define (iter-loop)
    (cond
      [(queue-empty? open-queue) '()]  ; No path found
      [else
       (define current (dequeue! open-queue))
       (cond
         ;; Goal reached?
         [(and (= (node-x current) (node-x goal))
               (= (node-y current) (node-y goal)))
          (reconstruct-path came-from current)]
         
         [else
          ;; Process current node
          (set-add! closed-set current)
          
          ;; Process neighbors
          (for ([neighbor (get-neighbors p current)])
            (when (and (not (set-member? closed-set neighbor))
                      (valid-move? neighbor obstacles))
              (define tentative-g-score 
                (add1 (hash-ref g-scores current 0)))
              
              (when (or (not (hash-has-key? g-scores neighbor))
                       (< tentative-g-score 
                          (hash-ref g-scores neighbor)))
                ;; Found better path, update scores
                (hash-set! came-from neighbor current)
                (hash-set! g-scores neighbor tentative-g-score)
                (hash-set! f-scores neighbor 
                          (+ tentative-g-score 
                             (heuristic neighbor goal)))
                (enqueue! open-queue 
                         (hash-ref f-scores neighbor) 
                         neighbor))))
          
          (iter-loop)])]))
  (iter-loop)) ;; Continue search

;; ;; Example usage and testing
;; (module+ test
;;   (require rackunit)
;;   
;;   ;; Basic test case
;;   (let* ([p (planner 10 10 20)]
;;          [start (node 0 0 0)]
;;          [goal (node 9 9 0)]
;;          [obstacles (set)])
;;     
;;     (test-case "Basic path finding"
;;       (define path (find-path p start goal obstacles))
;;       (check-true (not (empty? path)))
;;       (check-equal? (first path) start)
;;       (check-equal? (node-x (last path)) (node-x goal))
;;       (check-equal? (node-y (last path)) (node-y goal))))
;;   
;;   ;; Test with obstacles
;;   (let* ([p (planner 5 5 10)]
;;          [start (node 0 0 0)]
;;          [goal (node 4 4 0)]
;;          [obstacles (set (list 2 2 1)
;;                         (list 2 2 2)
;;                         (list 2 2 3))])
;;     
;;     (test-case "Path finding with obstacles"
;;       (define path (find-path p start goal obstacles))
;;       (check-true (not (empty? path)))
;;       (for ([node (rest path)])
;;         (check-true (valid-move? node obstacles))))))
;; 
;; ;; Documentation and usage example
;; (module+ main
;;   (define (run-example)
;;     (define p (planner 10 10 20))
;;     (define start (node 0 0 0))
;;     (define goal (node 9 9 0))
;;     (define obstacles 
;;       (set (list 2 2 1)
;;            (list 2 3 2)
;;            (list 3 2 3)))
;;     
;;     (printf "Finding path from ~a to ~a\n" 
;;             (list (node-x start) (node-y start))
;;             (list (node-x goal) (node-y goal)))
;;     
;;     (define path (find-path p start goal obstacles))
;;     
;;     (if (empty? path)
;;         (printf "No path found!\n")
;;         (begin
;;           (printf "Path found:\n")
;;           (for ([n path])
;;             (printf "  (~a,~a) at time ~a\n"
;;                     (node-x n)
;;                     (node-y n)
;;                     (node-t n))))))
;;   
;; ;;   (run-example))


;; Main Function
(define (part-A input exit)
  (define lines (string-split input "\n"))
;;  (define points (for/list ([line lines]) (string->point line)))
;;   (for ([y (add1 bound)])
;;     (for ([x (add1 bound)])
;;       (cond
;;         [(set-member? points (point x y)) (display #\#)]
;;         [else (display #\.)]))
;;     (displayln "")))
;;  (temporal-a-star exit exit points))
  lines)

;(part-A test 6)
;(part-A data 70)

;;

(define (part-B input)
  input)

;(part-B test)
;(part-B data)
