#lang racket

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

(define data (input->data "input/2024-12.txt"))

(define test "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

(struct plot [c x y] #:transparent)

(define (part-A L)
  (define lines (string-split L "\n"))

  (define all-plots (mutable-set))
  (for ([line lines][y (in-naturals)])
    (for ([ch line][x (in-naturals)])
      (set-add! all-plots (plot ch y x))))

  (define (valid-plot? p)
    (set-member? all-plots p))

  (define checked-plots (mutable-set))

  (define (next-plot)
    (for/or ([i (set->list all-plots)])
      (and (not (set-member? checked-plots i))
           i)))

  (define (get-neighbors p)
    (match-let ([(plot c x y) p])
      (let ([N (plot c x (add1 y))]
            [S (plot c x (sub1 y))]
            [E (plot c (add1 x) y)]
            [W (plot c (sub1 x) y)])
        (list N S E W))))
  
  (define (get-connected-plots start)
    (define this-group (mutable-set))
    (define plots-to-check (mutable-set))
    (define (dequeue! S)
      (let ([next (first (set->list S))])
        (set-remove! S next)
        next))
    (define (enqueue! S p)
      (set-add! S p))
    (set-add! plots-to-check start)
    (define (iter)
      (cond
        [(set-empty? plots-to-check) this-group]
        [else
         (let ([next (dequeue! plots-to-check)])
           (set-add! this-group next)
           (set-add! checked-plots next)
           (for ([p (get-neighbors next)])
             (when (and (valid-plot? p)
                        (not (set-member? this-group p)))
               (enqueue! plots-to-check p))))
         (iter)]))
    (iter))

  (define (get-all-connected)
    (define list-of-plots '())
    (define (iter)
      (cond
        [(not (next-plot)) list-of-plots]
        [else
         (let ([connected-plots (get-connected-plots (next-plot))])
           (set! list-of-plots (cons connected-plots list-of-plots))
           (iter))]))
    (iter))
  
  (define plots (get-all-connected))

  (define (fence-plot g)
    (* (length (set->list g))
       (for/sum ([p (in-set g)])
         (for/sum ([n (get-neighbors p)])
           (if (set-member? all-plots n) 0 1)))))

  (for/sum ([p plots]) (fence-plot p)))


(define test0 "AAAA
BBCD
BBCC
EEEC")

;(part-A test0)

(part-A test)
;(part-A data)

;;

(define (part-B L)
    (define lines (string-split L "\n"))

  (define all-plots (mutable-set))
  (for ([line lines][y (in-naturals)])
    (for ([ch line][x (in-naturals)])
      (set-add! all-plots (plot ch y x))))

  (define (valid-plot? p)
    (set-member? all-plots p))

  (define checked-plots (mutable-set))

  (define (next-plot)
    (for/or ([i (set->list all-plots)])
      (and (not (set-member? checked-plots i))
           i)))

  (define (get-neighbors p)
    (match-let ([(plot c x y) p])
      (let ([N (plot c x (add1 y))]
            [S (plot c x (sub1 y))]
            [E (plot c (add1 x) y)]
            [W (plot c (sub1 x) y)])
        (list N S E W))))
  
  (define (get-connected-plots start)
    (define this-group (mutable-set))
    (define plots-to-check (mutable-set))
    (define (dequeue! S)
      (let ([next (first (set->list S))])
        (set-remove! S next)
        next))
    (define (enqueue! S p)
      (set-add! S p))
    (set-add! plots-to-check start)
    (define (iter)
      (cond
        [(set-empty? plots-to-check) this-group]
        [else
         (let ([next (dequeue! plots-to-check)])
           (set-add! this-group next)
           (set-add! checked-plots next)
           (for ([p (get-neighbors next)])
             (when (and (valid-plot? p)
                        (not (set-member? this-group p)))
               (enqueue! plots-to-check p))))
         (iter)]))
    (iter))

  (define (get-all-connected)
    (define list-of-plots '())
    (define (iter)
      (cond
        [(not (next-plot)) list-of-plots]
        [else
         (let ([connected-plots (get-connected-plots (next-plot))])
           (set! list-of-plots (cons connected-plots list-of-plots))
           (iter))]))
    (iter))
  
  (define plots (get-all-connected))

  (define (corner-count p)
    (define (check-concave a b c)
      (if (and (set-member? all-plots a)
               (set-member? all-plots b)
               (not (set-member? all-plots c)))
          1
          0))
    (define (check-convex a b)
      (if (and (not (set-member? all-plots a))
               (not (set-member? all-plots b)))
          1
          0))
    (match-let ([(plot c x y) p])
      (let ([N (plot c x (add1 y))]
            [S (plot c x (sub1 y))]
            [E (plot c (add1 x) y)]
            [W (plot c (sub1 x) y)]
            [NE (plot c (add1 x) (add1 y))]
            [NW (plot c (sub1 x) (add1 y))]
            [SE (plot c (add1 x) (sub1 y))]
            [SW (plot c (sub1 x) (sub1 y))])
        (list (check-concave N E NE)
              (check-concave N W NW)
              (check-concave S E SE)
              (check-concave S W SW)
              (check-convex N E)
              (check-convex N W)
              (check-convex S E)
              (check-convex S W)))))
  
  (define (fence-plot g)
    (* (length (set->list g))
       (for/sum ([p (in-set g)])
         (apply + (corner-count p)))))

  (for/sum ([p plots]) (fence-plot p)))


(define test1 "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE")

(part-B test1)

(define test2 "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA
")

(part-B test2)

(part-B test)
(part-B data)
