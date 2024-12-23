#lang racket

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

;; Test data
(define data (input->data "input/2024-20.txt"))

(define test "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
")

;; Structs
(struct point [x y] #:transparent)

(define (manhatten-distance p1 p2)
  (match-let ([(point p1x p1y) p1][(point p2x p2y) p2])
    (+ (abs (- p2x p1x)) (abs (- p2y p1y)))))

;; Main Function
(define (part-A input)
  (define lines (string-split input "\n"))

  (define graph (mutable-set))
  (define start (point 0 0))
  (define end (point 0 0))
  
  (for ([line lines][y (in-naturals)])
    (for ([ch line][x (in-naturals)])
      (when (member ch (list #\. #\S #\E))
        (set-add! graph (point x y)))
      (when (char=? #\S ch)
        (set! start (point x y)))
      (when (char=? #\E ch)
        (set! end (point x y)))))

  (define (walk-path S)
    (define (iter p acc)
      (define (next)
        (match-let ([(point x y) p])
          (let ([E (point (add1 x) y)]
                [W (point (sub1 x) y)]
                [N (point x (add1 y))]
                [S (point x (sub1 y))])
            (cond
              [(and (set-member? graph E)
                    (not (member E acc)))
               E]
              [(and (set-member? graph W)
                    (not (member W acc)))
               W]
              [(and (set-member? graph N)
                    (not (member N acc)))
               N]
              [(and (set-member? graph S)
                    (not (member S acc)))
               S]))))
      (cond
        [(equal? p end) (cons p acc)]
        [else
         (iter (next) (cons p acc))]))
    (iter S '()))

  (define default-path (walk-path start))

  (define (count-shortcuts lop)
    (define (short-cut i wall path)
      (and (not (set-member? graph wall))
           (member path lop)
           (< i (index-of lop path))
           (- (index-of lop path) i 2)))
    (for/list ([p lop][i (in-naturals)])
      (match-let ([(point x y) p])
        (let ([E (point (add1 x) y)]
              [W (point (sub1 x) y)]
              [N (point x (add1 y))]
              [S (point x (sub1 y))]
              [EE (point (+ 2 x) y)]
              [WW (point (- x 2) y)]
              [NN (point x (+ 2 y))]
              [SS (point x (- y 2))])
          (map (λ (x) (if x x 0))
               (list (short-cut i E EE)
                     (short-cut i W WW)
                     (short-cut i N NN)
                     (short-cut i S SS)))))))

  (define shortcut-list (flatten (count-shortcuts default-path)))
  (define group-shortcut-list (group-by (λ (x) x) shortcut-list))
  (define group-length-list (map (λ (x) (list (first x) (length x))) group-shortcut-list))
  (sort group-length-list #:key car <))

(part-A test)

;; (let ([A-data (part-A data)])
;;   (for/sum ([i (filter (λ (x) (<= 100 (first x))) A-data)])
;;     (second i)))
  
;;

(define (part-B input)
  (define lines (string-split input "\n"))

  (define graph (mutable-set))
  (define start (point 0 0))
  (define end (point 0 0))
  
  (for ([line lines][y (in-naturals)])
    (for ([ch line][x (in-naturals)])
      (when (member ch (list #\. #\S #\E))
        (set-add! graph (point x y)))
      (when (char=? #\S ch)
        (set! start (point x y)))
      (when (char=? #\E ch)
        (set! end (point x y)))))

  (define (walk-path S)
    (define (iter p acc)
      (define (valid-pt? pt)
        (and (set-member? graph pt)
             (not (member pt acc))))
      (define (next)
        (match-let ([(point x y) p])
          (let ([E (point (add1 x) y)]
                [W (point (sub1 x) y)]
                [N (point x (add1 y))]
                [S (point x (sub1 y))])
            (cond
              [(valid-pt? E) E]
              [(valid-pt? W) W]
              [(valid-pt? N) N]
              [(valid-pt? S) S]))))
      (cond
        [(equal? p end) (cons p acc)]
        [else
         (iter (next) (cons p acc))]))
    (iter S '()))

  (define default-path (walk-path start))
  (define default-path-distance (for/hash ([p default-path][i (in-naturals)]) (values p i)))
  (define (distance p)
    (hash-ref default-path-distance p))

  (define (generate-points pt)
    (match-define (point px py) pt)
    (for*/list ([y (in-range -20 21)]
                [x (in-range -20 21)]
                #:when (and (<= (+ (abs x) (abs y)) 20)
                            (set-member? graph (point x y))
                            (< (+ (distance (point x y))
                                  (manhatten-distance (point x y) pt)
                                  -2)
                               (distance pt))))
      (point x y)))
  
  (define (find-short-cut p)
    (define possible-points (generate-points p))

    (define (diff a b)
      (- (distance a)
         (+ (manhatten-distance a b)
            (distance b))))
        
    (for/sum ([pp possible-points])
      (if (<= 50 (diff p pp)) 1 0)))

  (for/sum ([p (reverse default-path)])
    (find-short-cut p)))

(part-B test)
(part-B data) ; should be 1037501

