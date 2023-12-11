#lang racket
(require threading)
(require racket/match)
(require "util.rkt")

(define data (input->data "input/2023-11.txt"))

(define test "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(define test-B "....#........
.........#...
#............
.............
.............
........#....
.#...........
............#
.............
.............
.........#...
#....#.......")

;;

(struct point [x y] #:transparent)

(define (empty-row? str)
  (for/and ([c str]) (char=? #\. c)))

(define (transpose los)
  (for/list ([i (string-length (first los))])
    (~>> (for/list ([j (length los)])
           (string-ref (list-ref los j) i))
         (apply string))))

(define (expand-1D los lon acc)
  (cond
    [(empty? los) acc]
    [(and (not (empty? lon))
          (zero? (first lon)))
     (expand-1D (rest los)
                (map sub1 (rest lon))
                (cons (first los) (cons (first los) acc)))]
    [else
     (expand-1D (rest los)
                (map sub1 lon)
                (cons (first los) acc))]))

(define (expand L)
  (define rows (string-split L "\n"))
  (define empty-rows
    (for/list ([i (length rows)] #:when (empty-row? (list-ref rows i))) i))
  (define rows* (reverse (expand-1D rows empty-rows '())))

  (define cols (transpose rows*))
  (define empty-cols
    (for/list ([i (length cols)] #:when (empty-row? (list-ref cols i))) i))
  (define cols* (reverse (expand-1D cols empty-cols '())))
  (transpose cols*))
  
(define (part-A L)
  (define L* (expand L))
  (define rows L*)
  (define POINTS '())
  (for ([row rows][y (in-naturals)])
    (for ([c row][x (in-naturals)])
      (when (char=? #\# c)
        (set! POINTS (cons (point x y) POINTS)))))
  (for/sum ([ps (remove-duplicates (combinations POINTS 2))])
    (match-let ([(list (point x1 y1) (point x2 y2)) ps])
;;       (when (or (= x1 4) (= x2 4))
;;        (println (list ps (+ (abs (- x1 x2)) (abs (- y1 y2))))))
      (+ (abs (- x1 x2)) (abs (- y1 y2))))))

(part-A test)
(part-A data)

;;

(define (between? num a b)
  (< (min a b) num (max a b)))

(define (expand-2 L)
  (define rows (string-split L "\n"))
  (define empty-rows
    (for/list ([row rows][i (in-naturals)] #:when (empty-row? row)) i))

  (define cols (transpose rows))
  (define empty-cols
    (for/list ([col cols][i (in-naturals)] #:when (empty-row? col)) i))
  (values empty-rows empty-cols))

(define (part-B L)
  (define-values (empty-rows empty-cols) (expand-2 L))
  (define rows (string-split L "\n"))
  (define POINTS '())
  (for ([row rows][y (in-naturals)])
    (for ([c row][x (in-naturals)])
      (when (char=? #\# c)
        (set! POINTS (cons (point x y) POINTS)))))

  (for/sum ([ps (combinations POINTS 2)])
    (match-let ([(list (point x1 y1) (point x2 y2)) ps])
    (define expansion-rate (sub1 1000000))
    (let ([a (for/sum ([i empty-cols]) (if (between? i x1 x2) (* 1 expansion-rate) 0))]
          [b (for/sum ([i empty-rows]) (if (between? i y1 y2) (* 1 expansion-rate) 0))]
          [c (abs (- x1 x2))]
          [d (abs (- y1 y2))])
;;       (when (or (= x1 3) (= x2 3))
;;         (println (list (point x1 y1) (point x2 y2) empty-rows empty-cols (+ a b c d))))
      (+ a b c d))))
  )
 
(part-B test)
(part-B data)
