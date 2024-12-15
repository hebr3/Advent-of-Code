#lang racket
(require data/queue)
(require (only-in "util.rkt" input->data tap point))

(define data (input->data "input/2023-21.txt"))

(define test "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........")

;;


(define (part-A L steps)
  (let ([rows (string-split L "\n")]
        [G (make-hash)])
    (for ([row rows][y (in-naturals)])
      (for ([c row][x (in-naturals)])
        (match c
          [#\. (hash-set! G (point x y) #t)]
          [#\S (hash-set! G 'S (point x y))
               (hash-set! G (point x y) #t)]
          [#\# 'nothing])))

    (define (get-neighbors lop)
      (remove-duplicates
       (flatten
        (for/list ([p lop])
          (match-let ([(point x y) p])
            (list (point (sub1 x) y)
                  (point (add1 x) y)
                  (point x (sub1 y))
                  (point x (add1 y))))))))

    (define (valid-pt? pt)
      (hash-has-key? G pt))
    
    (define (print-steps lop)
      (println lop)
      (for ([row rows][y (in-naturals)])
        (for ([c row][x (in-naturals)])
          (let ([pt (point x y)])
            (cond
              [(member pt lop) (display #\O)]
              [(hash-has-key? G pt) (display #\.)]
              [else (display #\#)])))
        (displayln "")))
      
    (define (iter i acc)
      ;(print-steps acc)
      (cond
        [(zero? i) acc]
        [else
         (let* ([neighbors (get-neighbors acc)]
                [valid-neighbors (filter valid-pt? neighbors)])
           (iter (sub1 i) valid-neighbors))]))

    (length
     (iter steps (list (hash-ref G 'S))))))
    

(part-A test 6)
(part-A data 64)

;;


(define (part-B L)
  L)

;(part-B test)
;(part-B data)











