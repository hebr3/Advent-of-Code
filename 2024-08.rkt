#lang racket

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

(define data (input->data "input/2024-08.txt"))

(define test "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(struct point [x y] #:transparent)

(define (point<? p1 p2)
  (match-let ([(point x1 y1) p1]
              [(point x2 y2) p2])
    (or (< x1 x2)
        (and (= x1 x2)
             (< y1 y2)))))

(define (part-A L)
  (define LINES (string-split L "\n"))
  (define MAX-X (sub1 (string-length (first LINES))))
  (define MAX-Y (sub1 (length LINES)))

  (define (in-bounds? p)
    (match-let ([(point x y) p])
      (and (<= 0 x MAX-X)
           (<= 0 y MAX-Y))))
  
  (define NODES (make-hash))
  (for ([line LINES][r (in-naturals)])
    (for ([ch line][c (in-naturals)])
      (when (not (char=? #\. ch))
        (if (hash-has-key? NODES ch)
            (hash-set! NODES ch (cons (point c r) (hash-ref NODES ch)))
            (hash-set! NODES ch (list (point c r)))))))

  (define PAIRS (mutable-set))
  (for ([node-key (hash-keys NODES)])
    (let ([nodes (hash-ref NODES node-key)])
      (for* ([a nodes][b nodes] #:when (not (eq? a b)))
        (set-add! PAIRS (sort (list a b) point<?)))))

  (define ANTINODES (mutable-set))
  (for ([pair (set->list PAIRS)])
    (match-let* ([(list p1 p2) pair]
                 [(point x1 y1) p1]
                 [(point x2 y2) p2])
      (let ([dx (- x2 x1)]
            [dy (- y2 y1)])
        (set-add! ANTINODES (point (- x1 dx) (- y1 dy)))
        (set-add! ANTINODES (point (+ x2 dx) (+ y2 dy))))))
  
  (length (filter in-bounds? (set->list ANTINODES))))

(part-A test)
(part-A data)

;;

(define (part-B L)
  (define LINES (string-split L "\n"))
  (define MAX-X (sub1 (string-length (first LINES))))
  (define MAX-Y (sub1 (length LINES)))

  (define (in-bounds? p)
    (match-let ([(point x y) p])
      (and (<= 0 x MAX-X)
           (<= 0 y MAX-Y))))
  
  (define NODES (make-hash))
  (for ([line LINES][r (in-naturals)])
    (for ([ch line][c (in-naturals)])
      (when (not (char=? #\. ch))
        (if (hash-has-key? NODES ch)
            (hash-set! NODES ch (cons (point c r) (hash-ref NODES ch)))
            (hash-set! NODES ch (list (point c r)))))))

  (define PAIRS (mutable-set))
  (for ([node-key (hash-keys NODES)])
    (let ([nodes (hash-ref NODES node-key)])
      (for* ([a nodes][b nodes] #:when (not (eq? a b)))
        (set-add! PAIRS (sort (list a b) point<?)))))

  (define (all-antinodes p1 p2 HT)
    (match-let ([(point x1 y1) p1]
                [(point x2 y2) p2])
      (let ([dx (- x2 x1)][dy (- y2 y1)])
        (for ([i MAX-Y])
          (let ([new-pt (point (- x1 (* dx i)) (- y1 (* dy i)))])
            (when (in-bounds? new-pt)
              (set-add! HT new-pt)))
          (let ([new-pt (point (+ x2 (* dx i)) (+ y2 (* dy i)))])
            (when (in-bounds? new-pt)
              (set-add! HT new-pt)))))))
        
  
  (define ANTINODES (mutable-set))
  (for ([pair (set->list PAIRS)])
    (match-let* ([(list p1 p2) pair]
                 [(point x1 y1) p1]
                 [(point x2 y2) p2])
      (let* ([dx (- x2 x1)]
             [dy (- y2 y1)])
        (all-antinodes p1 p2 ANTINODES))))
  
  (length (filter in-bounds? (set->list ANTINODES))))

(part-B test)
(part-B data)
