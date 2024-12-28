#lang racket
(require threading)
(require 2htdp/image)

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))
(define data (input->data "input/2024-15.txt"))

(define test0 "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")

(define test "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")

(struct point [x y] #:transparent)

(struct Box [L R] #:transparent)

(define (part-B L)
  (match-define (list warehouse instructions) (string-split L "\n\n"))
  
  (define Walls (mutable-set))
  (define Boxes (mutable-set))
  (define robot (point 0 0))

  (define (in-Walls? p)
    (set-member? Walls p))
  (define (in-Boxes? p)
    (for/or ([b (set->list Boxes)])
      (match-let ([(Box L R) b])
        (or (equal? L p) (equal? R p)))))
  
  (for ([row (string-split warehouse)][y (in-naturals)])
    (for ([c row][x (in-naturals)])
      (cond
        [(char=? #\# c)
         (set-add! Walls (point (* 2 x) y))
         (set-add! Walls (point (add1 (* 2 x)) y))]
        [(char=? #\O c)
         (set-add! Boxes (Box (point (* 2 x) y) (point (add1 (* 2 x)) y)))]
        [(char=? #\@ c)
         (set! robot (point (* 2 x) y))]
        [else 'na])))

  (define (direction-fns d)
    (match d
      [#\^ (λ (p) (point (point-x p) (sub1 (point-y p))))]
      [#\> (λ (p) (point (add1 (point-x p)) (point-y p)))]
      [#\v (λ (p) (point (point-x p) (add1 (point-y p))))]
      [#\< (λ (p) (point (sub1 (point-x p)) (point-y p)))]))

  (define (find-box p)
    (for/or ([b Boxes])
      (match-let ([(Box L R) b])
        (if (member p (list L R))
            b
            #f))))

  (define (can-move-vertical? p dir)
    (cond
      [(not (in-Boxes? p)) #t]
      [else
       (let ([b (find-box p)])
         (match-let ([(Box L R) b])
           (let ([next-L ((direction-fns dir) L)]
                 [next-R ((direction-fns dir) R)])
             (cond
               [(or (in-Walls? next-L) (in-Walls? next-R)) #f]
               [else
                (and (can-move-vertical? next-L dir)
                     (can-move-vertical? next-R dir))]))))]))

  (define (move-Box? p dir)
    (cond
      [(in-Boxes? p)
       (let ([b (find-box p)])
         (match-let ([(Box L R) b])
           (let ([next-L ((direction-fns dir) L)]
                 [next-R ((direction-fns dir) R)])
             (cond
               [(or (in-Walls? next-L)
                    (in-Walls? next-R))
                #f]
               [(char=? #\< dir)
                (cond
                  [(move-Box? next-L dir)
                   (set-remove! Boxes b)
                   (set-add! Boxes (Box next-L next-R))
                   #t]
                  [else #f])]
               [(char=? #\> dir)
                (cond
                  [(move-Box? next-R dir)
                   (set-remove! Boxes b)
                   (set-add! Boxes (Box next-L next-R))
                   #t]
                  [else #f])]
               [(or (in-Boxes? next-L)
                    (in-Boxes? next-R))
                (cond
                  [(and (can-move-vertical? next-L dir)
                        (can-move-vertical? next-R dir))
                   (move-Box? next-L dir)
                   (move-Box? next-R dir)
                   (set-remove! Boxes b)
                   (set-add! Boxes (Box next-L next-R))
                   #t]
                  [else #f])]
               [else
                (set-remove! Boxes b)
                (set-add! Boxes (Box next-L next-R))
                #t]))))]
      [else #t]))
  
  (define (move-robot dir)
    (match-let ([(point x y) robot])
      (let ([next ((direction-fns dir) robot)])
        (cond
          [(in-Walls? next) #f]
          [(in-Boxes? next)
           (cond
             [(move-Box? next dir)
              (set! robot next)]
             [else #f])]
          [else
           (set! robot next)]))))

  (define (move d)
    ;(println (format "Move ~a:" d))
    (when (member d (list #\^ #\> #\v #\<))
      (move-robot d)))
  
  (define (left-Boxes)
    (map Box-L (set->list Boxes)))
  
  (define (right-Boxes)
    (map Box-R (set->list Boxes)))
  
  (define (print-warehouse)
    (for ([row (string-split warehouse)][y (in-naturals)])
      (for ([c (* 2 (string-length row))][x (in-naturals)])
        (cond
          [(in-Walls? (point x y)) (display #\#)]
          [(member (point x y) (left-Boxes))
           (display #\[)]
          [(member (point x y) (right-Boxes))
           (display #\])]
          [(equal? robot (point x y)) (display #\@)]
          [else (display #\.)]))
      (displayln ""))
    (displayln "")
    )

  ;(print-warehouse)

  (for ([c instructions][i (in-naturals)])
    (move c))
  ;(print-warehouse)
  (for/sum ([b (in-mutable-set Boxes)])
    (match-let* ([(Box L R) b]
                 [(point x y) L])
      (+ x (* 100 y)))))

(part-B test)
(part-B data)