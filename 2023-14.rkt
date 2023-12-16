#lang racket
(require (only-in "util.rkt" input->data tap point))

(define data (input->data "input/2023-14.txt"))

(define test "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(define test2 ".....#....
....#...O#
.....##...
...#......
.....OOO#.
.O#...O#.#
....O#...O
......OOOO
#...O###.O
#..OO#..OO")

;;

;; (define (transpose los)
;;   (let ([Y (length los)]
;;         [X (string-length (first los))])
;;     (for/list ([x X])
;;       (apply string
;;              (for/list ([y Y])
;;                (string-ref (list-ref los y) x))))))
;; 
;; (define (score-rows los)
;;   (let ([Y (length los)]
;;         [X (string-length (first los))])
;;     (for*/sum ([y Y][x X])
;;       (if (char=? #\O (string-ref (list-ref los y) x))
;;           (- X x)
;;           0))))
;; 
;; (define (part-A L)
;;   (let* ([los (string-split L "\n")]
;;          [rows (transpose los)]
;;          [Y (length rows)]
;;          [X (string-length (first rows))]
;;          [ROUND-ROCKS '()]
;;          [CUBE-ROCKS '()])
;;     (define (adjust row)
;;       (define (iter str i)
;;         (if (= i (sub1 (string-length str)))
;;             str
;;             (let ([c0 (string-ref str i)]
;;                   [c1 (string-ref str (add1 i))])
;;               (cond
;;                 [(or (char=? #\O c0) (char=? #\# c0))
;;                  (iter str (add1 i))]
;;                 [(and (char=? #\. c0)
;;                       (char=? #\O c1))
;;                  (let ([str* (string-copy str)])
;;                    (string-set! str* i #\O)
;;                    (string-set! str* (add1 i) #\.)
;;                    (iter str* 0))]
;;                 [else
;;                  (iter str (add1 i))]))))
;;       (iter row 0))
;;     (score-rows (for/list ([row rows]) (adjust row)))))
;;         
;; 
;; (part-A test)
;; (part-A data)

;;

(define (str->loloc str)
  (let ([rows (string-split str "\n")])
    (for/list ([row rows])
      (for/list ([col row]) col))))

(define (rotate-ccw mat)
  (let ([Y (length mat)]
        [X (length (first mat))])
    (for/list ([x X])
      (for/list ([y Y])
        (list-ref (list-ref mat y) (- X x 1))))))

(define (rotate-ccw-turns mat turns)
  (define (iter m* i)
    (if (<= i 0)
        m*
        (iter (rotate-ccw m*) (sub1 i))))
  (iter mat turns))

(define (list-swap L i)
  (for/list ([x (length L)])
    (cond
      [(= x i) (list-ref L (add1 x))]
      [(= x (add1 i)) (list-ref L (sub1 x))]
      [else (list-ref L x)])))

(define (adjust mat)
  (define (iter loc i)
    (if (= i (sub1 (length loc)))
        loc
        (let ([c0 (list-ref loc i)]
              [c1 (list-ref loc (add1 i))])
          (cond
            [(or (char=? #\O c0) (char=? #\# c0))
             (iter loc (add1 i))]
            [(and (char=? #\. c0)
                  (char=? #\O c1))
             (iter (list-swap loc i) 0)]
            [else
             (iter loc (add1 i))]))))
  (for/list ([row mat])
    (iter row 0)))

(define (north mat)
  (rotate-ccw-turns (adjust (rotate-ccw-turns mat 1)) 3))
(define (west mat)
  (rotate-ccw-turns (adjust (rotate-ccw-turns mat 0)) 4))
(define (south mat)
  (rotate-ccw-turns (adjust (rotate-ccw-turns mat 3)) 1))
(define (east mat)
  (rotate-ccw-turns (adjust (rotate-ccw-turns mat 2)) 2))

(define (cycle-mat mat DP DP-C i)
  (hash-set! DP-C mat i)
  (cond
    [(hash-has-key? DP (mat->str mat))
     (hash-ref DP (mat->str mat))]
    [else
     (let ([new (east (south (west (north mat))))])
       (hash-set! DP (mat->str mat) new)
       new)]))

(define (tap-mat mat)
  (for ([r mat])
    (for ([c r])
      (display c))
    (displayln ""))
  (displayln "")
  mat)

(define (tap-mats old new)
  (for ([o old][n new])
    (display-row o)
    (display " ")
    (display-row n)
    (displayln ""))
  (displayln ""))

(define (display-row row)
  (for ([c row]) (display c)))

(define (mat->str mat)
  (string-join (map (λ (r) (apply string r)) mat)))

(define (score-mat mat)
  (let ([Y (length mat)]
        [X (length (first mat))])
    (for*/sum ([y Y][x X])
      (if (char=? #\O (list-ref (list-ref mat y) x))
          (- Y y)
          0))))

(define (part-B L)
  (let ([DP-next (make-hash)]
        [DP-count (make-hash)]
        [mat (str->loloc L)])
    (define (iter m i)
      (cond
        [(< 10000 i)
         (println "I went over 1000")
         DP-next]
        [(hash-has-key? DP-next (mat->str m))
         (println (format "Found a loop in ~a" i))
         m]
        [else
         (let* ([old m]
                [new (cycle-mat old DP-next DP-count i)])
           (iter new (add1 i)))]))
    (define A (iter mat 0))
    (define loop-start (hash-ref DP-count A))
    (define loop-length (- (hash-count DP-count) (hash-ref DP-count A)))
    (define billion-idx (modulo (- 1000000000 loop-start) loop-length))
    (displayln (format "loop start:  ~a" loop-start))
    (displayln (format "loop length: ~a" loop-length))
    (displayln (format "(1,000,000,000 - ~a) % ~a = ~a" loop-start loop-length billion-idx))
    
    (for ([i (add1 billion-idx)])
      ;(display (hash-ref DP-count A))
      ;(for ([s A]) (display-row s))
      ;(displayln "")
      (when (= i billion-idx)
        (displayln (score-mat A)))
      (set! A (hash-ref DP-next (mat->str A))))
    ))

(part-B test)
;(part-B test2)
;(part-B data)
