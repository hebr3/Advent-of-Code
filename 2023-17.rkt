#lang racket
(require graph)
(require data/heap)
(require (only-in "util.rkt" input->data tap point))

(define data (input->data "input/2023-17.txt"))

(define test "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
")

;;

(define (make-matrix str)
  (let ([rows (string-split str "\n")])
    (for/list ([row rows])
      (for/list ([c row])
        (string->number (string c))))))

(define (mat-ref mat r c)
  (list-ref (list-ref mat r) c))

(struct node [distance r c direction count-in-direction] #:transparent)

(define (node<=? n1 n2)
  (match-let ([(node distance1 r1 c1 direction1 count-in-direction1) n1]
              [(node distance2 r2 c2 direction2 count-in-direction2) n2])
    (<= distance1 distance2)))

(define (node->hash-val n)
  (match-let ([(node distance r c direction count-in-direction) n])
    (list r c direction count-in-direction)))

(define (head-pop?! bh)
  (cond
    [(zero? (heap-count bh)) #f]
    [else
     (let ([head (heap-min bh)])
       (heap-remove-min! bh)
       head)]))

(define (hash-sort<=? one two)
  (match-let ([(cons (list r1 c1 direction1 count-in-direction1) _) one]
              [(cons (list r2 c2 direction2 count-in-direction2) _) two])
    (cond
      [(< r1 r2) #t]
      [(> r1 r2) #f]
      [(< c1 c2) #t]
      [(> c1 c2) #f]
      [(< direction1 direction2) #t]
      [(> direction1 direction2) #f]
      [(< count-in-direction1 count-in-direction2) #t]
      [(> count-in-direction1 count-in-direction2) #f])))

(define (part-A L)
  (let* ([MATRIX (make-matrix L)]
         [ROWS (length MATRIX)]
         [COLUMNS (length (first MATRIX))]
         [PRIORITY-QUEUE (make-heap node<=?)]
         [SEEN (make-hash)]
         [start-point (node 0 0 0 -1 -1)])
    (heap-add! PRIORITY-QUEUE start-point)
    (define (iter)
      (let ([head (head-pop?! PRIORITY-QUEUE)])
        (cond
          [(not head) (displayln "ran out the heap")]
          [(hash-has-key? SEEN (node->hash-val head)) (iter)]
          [else
           (match-let ([(node distance r c direction count-in-direction) head])
             (hash-set! SEEN (node->hash-val head) distance)
             ;(displayln (node->hash-val head))
             (for ([i (in-naturals)] [dr '(-1 0 1 0)] [dc '(0 1 0 -1)])
               (let* ([new-r (+ r dr)]
                      [new-c (+ c dc)]
                      [new_dir i]
                      [new_indir (if (= direction new_dir) (add1 count-in-direction) 1)]
                      [isnt_reverse (not (= (modulo (+ new_dir 2) 4) direction))]
                      [isvalid (<= new_indir 3)])
                 (when (and (<= 0 new-r (sub1 ROWS)) (<= 0 new-c (sub1 COLUMNS)) isnt_reverse isvalid)
                   (define cost (mat-ref MATRIX new-r new-c))
                   (define new_pt (node (+ distance cost) new-r new-c new_dir new_indir))
                   (heap-add! PRIORITY-QUEUE new_pt))))
             (iter))])))
    (iter)
    (cdr (last (sort (hash->list SEEN) hash-sort<=?)))))

(part-A test)
(part-A data)

;;

(define test1 "111111111111
999999999991
999999999991
999999999991
999999999991")

(define (part-B L)
  (let* ([MATRIX (make-matrix L)]
         [ROWS (length MATRIX)]
         [COLUMNS (length (first MATRIX))]
         [PRIORITY-QUEUE (make-heap node<=?)]
         [SEEN (make-hash)]
         [start-point (node 0 0 0 -1 -1)])
    (heap-add! PRIORITY-QUEUE start-point)
    (define (iter)
      (let ([head (head-pop?! PRIORITY-QUEUE)])
        (cond
          [(not head) (displayln "ran out the heap")]
          [(hash-has-key? SEEN (node->hash-val head)) (iter)]
          [else
           (match-let ([(node distance r c direction_ count-in-direction) head])
             (hash-set! SEEN (node->hash-val head) distance)
             ;(displayln (node->hash-val head))
             (for ([i (in-naturals)] [dr '(-1 0 1 0)] [dc '(0 1 0 -1)])
               (let* ([new-r (+ r dr)]
                      [new-c (+ c dc)]
                      [new_dir i]
                      [new_indir (if (= direction_ new_dir) (add1 count-in-direction) 1)]
                      [isnt_reverse (not (= (modulo (+ new_dir 2) 4) direction_))]
                      [isvalid (and (<= new_indir 10)
                                    (or (= new_dir direction_) (>= count-in-direction 4) (= count-in-direction -1)))])
                 (when (and (<= 0 new-r (sub1 ROWS)) (<= 0 new-c (sub1 COLUMNS)) isnt_reverse isvalid)
                   (define cost (mat-ref MATRIX new-r new-c))
                   (define new_pt (node (+ distance cost) new-r new-c new_dir new_indir))
                   (heap-add! PRIORITY-QUEUE new_pt))))
             (iter))])))
    (iter)
    (cdr (last (sort (hash->list SEEN) hash-sort<=?)))))

(part-B test)
(part-B test1)
(part-B data)