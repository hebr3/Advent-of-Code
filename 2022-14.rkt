#lang racket
(require threading)
(require racket/match)
(require graph)

;;;;;

(define data
  (~> "input/2022-14.txt"
      open-input-file
      (read-line _ 'return)
      ))

(define test "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

;;;;;

(struct point [x y] #:transparent)

;;;;;

(define (fill-points p1 p2)
  (match-let ([(point x1 y1) p1][(point x2 y2) p2])
    (cond
      [(and (= y1 y2) (< x1 x2))
       (for/list ([i (range x1 (add1 x2))])
         (point i y1))]
      [(= y1 y2)
       (for/list ([i (range x2 (add1 x1))])
         (point i y1))]
      [(< y1 y2)
       (for/list ([i (range y1 (add1 y2))])
         (point x1 i))]
      [else
       (for/list ([i (range y2 (add1 y1))])
         (point x1 i))])))

(define (build-line LoP)
  (cond
    [(empty? LoP) '()]
    [(= 1 (length LoP)) '()]
    [else
     (append (fill-points (first LoP) (second LoP))
             (build-line (rest LoP)))]))

(define (parse-line str)
  (define LoS (string-split str " -> "))
  (define (string->point str)
    (match-let ([(list x y) (string-split str ",")])
      (point (string->number x) (string->number y))))
  (map string->point LoS))

(define (build-grid LoLoP)
  (remove-duplicates (flatten (map build-line LoLoP))))

(define (draw-grid LoP LoW)
  (define xs (map point-x LoP))
  (define ys (map point-y LoP))
  (define x-min (apply min xs))
  (define x-max (apply max xs))
  (define y-min (apply min ys))
  (define y-max (apply max ys))
  (for ([j (add1 (- y-max y-min))])
    (for ([i (add1 (- x-max x-min))])
      (cond
        [(member (point (+ i x-min) (+ j y-min)) LoW)
         (display "#")]
        [(member (point (+ i x-min) (+ j y-min)) LoP)
         (display "O")]
        [else
         (display " ")]))
    (displayln ""))
  (displayln ""))

;;;;;

(define (find-sand-loc LoP)
  (define xs (map point-x LoP))
  (define ys (map point-y LoP))
  (define x-min (apply min xs))
  (define x-max (apply max xs))
  (define y-min (apply min ys))
  (define y-max (apply max ys))
  (define (iter pt)
    (match-let ([(point x y) pt])
      (cond
        [(< y-max y) pt]
        [(not (member (point x (add1 y)) LoP))
         (iter (point x (add1 y)))]
        [(not (member (point (sub1 x) (add1 y)) LoP))
         (iter (point (sub1 x) (add1 y)))]
        [(not (member (point (add1 x) (add1 y)) LoP))
         (iter (point (add1 x) (add1 y)))]
        [else
         pt])))
  (iter (point 500 (sub1 y-min))))

(define (lowest-pt? pt LoP)
  (match-let ([(point _ y) pt])
    (define y-max (apply max (map point-y LoP)))
    (< y-max y)))

(define (fill LoP)
  (define (iter pt L*)
    ;(draw-grid L* LoP)
    (cond
      [(lowest-pt? pt L*) L*]
      [else
       (let ([newPt (find-sand-loc L*)])
         (iter newPt (cons pt L*)))]))
  (iter (find-sand-loc LoP) LoP))

(define (part-A L)
  (define grid (build-grid (map parse-line (string-split L "\n"))))
  (define endState (fill grid))
  ;(draw-grid endState grid)
  (length (set->list (set-subtract (apply set endState)
                                   (apply set grid))))
  )

;;;;;

(part-A test)
(part-A data)

;;;;;

(define (draw-grid2 LoP LoW)
  (define xs (map point-x (set->list LoP)))
  (define ys (map point-y (set->list LoP)))
  (define x-min (apply min xs))
  (define x-max (apply max xs))
  (define y-min (apply min ys))
  (define y-max (apply max ys))
  (for ([j (add1 (- y-max y-min))])
    (for ([i (add1 (- x-max x-min))])
      (cond
        [(set-member? LoW (point (+ i x-min) (+ j y-min)))
         (display "#")]
        [(set-member? LoP (point (+ i x-min) (+ j y-min)))
         (display "O")]
        [else
         (display " ")]))
    (displayln ""))
  (displayln ""))

(define (find-sand-loc2 SoP SoW)
  (define xs (map point-x (set->list SoP)))
  (define ys (map point-y (set->list SoP)))
  (define x-min (apply min xs))
  (define x-max (apply max xs))
  (define y-min (apply min ys))
  (define y-max (apply max (map point-y (set->list SoW))))
  (define (iter pt)
    (match-let ([(point x y) pt])
      (define L (point (sub1 x) (add1 y)))
      (define M (point x (add1 y)))
      (define R (point (add1 x) (add1 y)))
      (cond
        [(< y-max y) pt]
        [(not (set-member? SoP (point x (add1 y))))
         (iter (point x (add1 y)))]
        [(not (set-member? SoP (point (sub1 x) (add1 y))))
         (iter (point (sub1 x) (add1 y)))]
        [(not (set-member? SoP (point (add1 x) (add1 y))))
         (iter (point (add1 x) (add1 y)))]
        [else
         pt])))
  (iter (point 500 (sub1 y-min))))

(define (fill2 SoP)
  (define (iter pt L*)
    (cond
      [(zero? (point-y pt)) (set-add L* pt)]
      [else
       (iter (find-sand-loc2 L* SoP) (set-add L* pt))]))
  (iter (find-sand-loc2 SoP SoP) SoP))

(define (part-B L)
  (define grid (build-grid (map parse-line (string-split L "\n"))))
  (define endState (fill2 (apply set grid)))
  ;(draw-grid2 endState (apply set grid))
  (length (set->list (set-subtract endState (apply set grid)))))
  

(part-B test)
(part-B data)
