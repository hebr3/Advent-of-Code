#lang racket
(require racket/match)
(require threading)
(require racket/set)
(require (only-in "util.rkt" input->data tap point))

(define data (input->data "input/2023-16.txt"))

(define test
  ".V...B....
V.H.B.....
.....VH...
........V.
..........
.........B
....F.BB..
.H.HF..V..
.V....HV.B
..FF.V....")

;;

(define (mirrors los)
  ;(println 'mirrors)
  (let ([MIRRORS (make-hash)])
    (for* ([y (length los)] [x (string-length (first los))])
      (let ([ch (string-ref (list-ref los y) x)])
        (cond
          [(char=? #\B ch) (hash-set! MIRRORS (point x y) 'backslash)]
          [(char=? #\F ch) (hash-set! MIRRORS (point x y) 'forwardslash)]
          [(char=? #\V ch) (hash-set! MIRRORS (point x y) 'vertical)]
          [(char=? #\H ch) (hash-set! MIRRORS (point x y) 'horizontal)])))
    MIRRORS))

(define (move-pt dir pt)
  (match-let ([(point x y) pt])
    (cond
      [(equal? 'up dir) (point x (sub1 y))]
      [(equal? 'down dir) (point x (add1 y))]
      [(equal? 'left dir) (point (sub1 x) y)]
      [(equal? 'right dir) (point (add1 x) y)])))

(define (member? v lst)
  (and (member v lst) #t))

(define (part-A L)
  (let* ([rows (string-split L "\n")]
         [Y (length rows)]
         [X (string-length (first rows))]
         [MIRRORS (mirrors rows)]
         [ENERGIZED-up (make-hash)]
         [ENERGIZED-down (make-hash)]
         [ENERGIZED-left (make-hash)]
         [ENERGIZED-right (make-hash)])
    (define (energize dir pt)
      (cond
        [(equal? 'up dir) (hash-update! ENERGIZED-up pt add1 0)]
        [(equal? 'down dir) (hash-update! ENERGIZED-down pt add1 0)]
        [(equal? 'left dir) (hash-update! ENERGIZED-left pt add1 0)]
        [(equal? 'right dir) (hash-update! ENERGIZED-right pt add1 0)]))
    (define (energized? dir pt)
      (cond
        [(equal? 'up dir) (hash-has-key? ENERGIZED-up pt)]
        [(equal? 'down dir) (hash-has-key? ENERGIZED-down pt)]
        [(equal? 'left dir) (hash-has-key? ENERGIZED-left pt)]
        [(equal? 'right dir) (hash-has-key? ENERGIZED-right pt)]))
    (define (move-pt pt dir)
      (match-let ([(point x y) pt])
        (cond
          [(equal? 'up dir) (point x (sub1 y))]
          [(equal? 'down dir) (point x (add1 y))]
          [(equal? 'left dir) (point (sub1 x) y)]
          [(equal? 'right dir) (point (add1 x) y)])))
    (define (beam dir pt)
      (match-let ([(point x y) pt])
        (let ([m? (hash-has-key? MIRRORS pt)] [e? (energized? dir pt)])
          (cond
            [(< x 0) #f];(println (list 'off-left dir pt))]
            [(< y 0) #f];(println (list 'off-up dir pt))]
            [(<= X x) #f];(println (list 'off-right dir pt))]
            [(<= Y y) #f];(println (list 'off-down dir pt))]
            [e? #f];(println (list 'repeat dir pt))]
            [(not m?)
             (energize dir pt)
             (beam dir (move-pt pt dir))]
            [else
             (let ([M (hash-ref MIRRORS pt)])
               (energize dir pt)
               (cond
                 ;; | vertical
                 [(equal? 'vertical M)
                  (cond
                    [(member pt (list 'up 'down)) (beam dir (move-pt pt dir))]
                    [else
                     (beam 'up (move-pt pt 'up))
                     (beam 'down (move-pt pt 'down))])]

                 ;; - horizontal
                 [(equal? 'horizontal M)
                  (cond
                    [(member pt (list 'right 'left)) (beam dir (move-pt pt dir))]
                    [else
                     (beam 'right (move-pt pt 'right))
                     (beam 'left (move-pt pt 'left))])]

                 ;; \ backslash
                 [(equal? 'backslash M)
                  (cond
                    [(equal? 'up dir) (beam 'left (move-pt pt 'left))]
                    [(equal? 'down dir) (beam 'right (move-pt pt 'right))]
                    [(equal? 'left dir) (beam 'up (move-pt pt 'up))]
                    [(equal? 'right dir) (beam 'down (move-pt pt 'down))])]

                 ;; / forwardslash
                 [(equal? 'forwardslash M)
                  (cond
                    [(equal? 'down dir) (beam 'left (move-pt pt 'left))]
                    [(equal? 'up dir) (beam 'right (move-pt pt 'right))]
                    [(equal? 'right dir) (beam 'up (move-pt pt 'up))]
                    [(equal? 'left dir) (beam 'down (move-pt pt 'down))])]

                 [else #f];(println (list 'more-options dir pt))]
                 ))]))))
    (beam 'right (point 0 0))
    (displayln L)
    (define (has-been-energized? pt)
      (for/or ([dir (list 'up 'down 'left 'right)])
        (energized? dir pt)))
    (for ([y Y])
      (for ([x X])
        (if (has-been-energized? (point x y)) (display #\#) (display #\.)))
      (displayln ""))

    (for/sum ([y Y]) (for/sum ([x X]) (if (has-been-energized? (point x y)) 1 0)))))

(part-A test)
;;(part-A data)

;;

(define (part-B L)
  (define (iter start-dir start-pt)
    (let* ([rows (string-split L "\n")]
           [Y (length rows)]
           [X (string-length (first rows))]
           [MIRRORS (mirrors rows)]
           [ENERGIZED-up (make-hash)]
           [ENERGIZED-down (make-hash)]
           [ENERGIZED-left (make-hash)]
           [ENERGIZED-right (make-hash)])
      (define (energize dir pt)
        (cond
          [(equal? 'up dir) (hash-update! ENERGIZED-up pt add1 0)]
          [(equal? 'down dir) (hash-update! ENERGIZED-down pt add1 0)]
          [(equal? 'left dir) (hash-update! ENERGIZED-left pt add1 0)]
          [(equal? 'right dir) (hash-update! ENERGIZED-right pt add1 0)]))
      (define (energized? dir pt)
        (cond
          [(equal? 'up dir) (hash-has-key? ENERGIZED-up pt)]
          [(equal? 'down dir) (hash-has-key? ENERGIZED-down pt)]
          [(equal? 'left dir) (hash-has-key? ENERGIZED-left pt)]
          [(equal? 'right dir) (hash-has-key? ENERGIZED-right pt)]))
      (define (move-pt pt dir)
        (match-let ([(point x y) pt])
          (cond
            [(equal? 'up dir) (point x (sub1 y))]
            [(equal? 'down dir) (point x (add1 y))]
            [(equal? 'left dir) (point (sub1 x) y)]
            [(equal? 'right dir) (point (add1 x) y)])))
      (define (beam dir pt)
        (match-let ([(point x y) pt])
          (let ([m? (hash-has-key? MIRRORS pt)] [e? (energized? dir pt)])
            (cond
              [(< x 0) #f];(println (list 'off-left dir pt))]
              [(< y 0) #f];(println (list 'off-up dir pt))]
              [(<= X x) #f];(println (list 'off-right dir pt))]
              [(<= Y y) #f];(println (list 'off-down dir pt))]
              [e? #f];(println (list 'repeat dir pt))]
              [(not m?)
               (energize dir pt)
               (beam dir (move-pt pt dir))]
              [else
               (let ([M (hash-ref MIRRORS pt)])
                 (energize dir pt)
                 (cond
                   ;; | vertical
                   [(equal? 'vertical M)
                    (cond
                      [(member pt (list 'up 'down)) (beam dir (move-pt pt dir))]
                      [else
                       (beam 'up (move-pt pt 'up))
                       (beam 'down (move-pt pt 'down))])]

                   ;; - horizontal
                   [(equal? 'horizontal M)
                    (cond
                      [(member pt (list 'right 'left)) (beam dir (move-pt pt dir))]
                      [else
                       (beam 'right (move-pt pt 'right))
                       (beam 'left (move-pt pt 'left))])]

                   ;; \ backslash
                   [(equal? 'backslash M)
                    (cond
                      [(equal? 'up dir) (beam 'left (move-pt pt 'left))]
                      [(equal? 'down dir) (beam 'right (move-pt pt 'right))]
                      [(equal? 'left dir) (beam 'up (move-pt pt 'up))]
                      [(equal? 'right dir) (beam 'down (move-pt pt 'down))])]

                   ;; / forwardslash
                   [(equal? 'forwardslash M)
                    (cond
                      [(equal? 'down dir) (beam 'left (move-pt pt 'left))]
                      [(equal? 'up dir) (beam 'right (move-pt pt 'right))]
                      [(equal? 'right dir) (beam 'up (move-pt pt 'up))]
                      [(equal? 'left dir) (beam 'down (move-pt pt 'down))])]

                   [else #f];(println (list 'more-options dir pt))]
                   ))]))))
      (beam start-dir start-pt)
;;       (displayln L)
      (define (has-been-energized? pt)
        (for/or ([dir (list 'up 'down 'left 'right)])
          (energized? dir pt)))
;;       (for ([y Y])
;;         (for ([x X])
;;           (if (has-been-energized? (point x y)) (display #\#) (display #\.)))
;;         (displayln ""))

      (for/sum ([y Y]) (for/sum ([x X]) (if (has-been-energized? (point x y)) 1 0)))))
  (let* ([rows (string-split L "\n")]
         [Y (length rows)]
         [X (string-length (first rows))])
    (apply max (flatten (list (for/list ([y Y]) (iter 'right (point 0 y)))
                              (for/list ([x X]) (iter 'down (point x 0)))
                              (for/list ([y Y]) (iter 'left (point (sub1 X) y)))
                              (for/list ([x X]) (iter 'up (point x (sub1 Y)))))))
           ))

(part-B test)
(part-B data)
