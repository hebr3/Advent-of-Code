#lang racket
(require data/queue)
(require (only-in "util.rkt" input->data tap point))

(define data (input->data "input/2023-23.txt"))

(define test "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#")

;;

(define (longest-path-DFS graph start end visited current-path)
  (cond
    [(equal? start end) current-path]
    [else
     (let ([new-visited (cons start visited)]
           [longest-path '()])
       (for ([neighbor (hash-ref graph start)])
         (when (not (member neighbor new-visited))
           (let ([new-path (longest-path-DFS graph neighbor end new-visited (cons current-path neighbor))])
             (when (and (not (empty? new-path))
                        (or (empty? longest-path)
                            (> (length (flatten new-path)) (length longest-path))))
               (set! longest-path (flatten new-path))
               (println (list (current-seconds) (length longest-path)))))))
       longest-path)]))

(define (part-A L)
  (let* ([rows (string-split L "\n")]
         [R (length rows)]
         [C (string-length (first rows))]
         [G (make-hash)])
    (for ([row rows][r R])
      (for ([ch row][c C])
        (when (char=? ch #\.)
          (hash-set! G (point r c) (list)))))
    (for ([r R])
      (for ([c (sub1 C)])
        (when (and (hash-has-key? G (point r c))
                   (hash-has-key? G (point r (add1 c))))
          (hash-set! G (point r c) (cons (point r (add1 c)) (hash-ref G (point r c))))
          (hash-set! G (point r (add1 c)) (cons (point r c) (hash-ref G (point r (add1 c))))))))
    (for ([r (sub1 R)])
      (for ([c C])
        (when (and (hash-has-key? G (point r c))
                   (hash-has-key? G (point (add1 r) c)))
          (hash-set! G (point r c) (cons (point (add1 r) c) (hash-ref G (point r c))))
          (hash-set! G (point (add1 r) c) (cons (point r c) (hash-ref G (point (add1 r) c)))))))
    (for ([row rows][r R])
      (for ([ch row][c C])
        (let ([pt (point r c)]
              [up (point (sub1 r) c)]
              [left (point r (sub1 c))]
              [down (point (add1 r) c)]
              [right (point r (add1 c))])
        (when (char=? #\> ch)
          (hash-set! G left (cons pt (hash-ref G left)))
          (hash-set! G pt (cons right '())))
        (when (char=? #\< ch)
          (hash-set! G right (cons pt (hash-ref G right)))
          (hash-set! G pt (cons left '())))
        (when (char=? #\^ ch)
          (hash-set! G down (cons pt (hash-ref G down)))
          (hash-set! G pt (cons up '())))
        (when (char=? #\v ch)
          (hash-set! G up (cons pt (hash-ref G up)))
          (hash-set! G pt (cons down '()))))))

    (define (print-path lop)
      (for ([row rows][r R])
        (for ([ch row][c C])
          (if (member (point r c) lop)
              (display #\O)
              (display ch)))
        (displayln "")))
            
    (let ([path (longest-path-DFS G (point 0 1) (point (sub1 R) (- C 2)) '() (list (point 0 1)))])
      ;(print-path path)
      (println (sub1 (length path))))))

(part-A test)
;(part-A data)

;;

(define (part-B L)
  (let ([new-L (string-replace (string-replace (string-replace L ">" ".") "<" ".") "v" ".")])
    (part-A new-L)))

(part-B test)
(part-B data)








