#lang racket
(require threading)
(require 2htdp/image)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

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
(define (part-A L)
  (match-define (list warehouse instructions) (string-split L "\n\n"))
  (define walls (mutable-set))
  (define boxes (mutable-set))
  (define robot (point 0 0))
  
  (for ([row (string-split warehouse)][y (in-naturals)])
    (for ([c row][x (in-naturals)])
      (cond
        [(char=? #\# c) (set-add! walls (point x y))]
        [(char=? #\O c) (set-add! boxes (point x y))]
        [(char=? #\@ c) (set! robot (point x y))]
        [else 'na])))
  
  (define (point-dir p dir)
    (match-let ([(point x y) p])
      (match dir
        [#\^ (point x (sub1 y))]
        [#\> (point (add1 x) y)]
        [#\v (point x (add1 y))]
        [#\< (point (sub1 x) y)])))

  (define (point-N p) (point-dir p #\^))
  (define (point-E p) (point-dir p #\>))
  (define (point-S p) (point-dir p #\v))
  (define (point-W p) (point-dir p #\<))
  
  ;; NORTH
  (define (move-box-^? p)
    (match-let ([(point x y) p])
      (let ([next (point-N p)])
        (cond
          [(set-member? walls next)
           ;(println (list "can't move NORTH wall" p))
           #f]
          [(set-member? boxes next)
           (cond
             [(move-box-^? next)
              ;(println (list "moving box NORTH blocker" p))
              (set-remove! boxes p)
              (set-add! boxes next)
              #t]
             [else
              ;(println (list "can't move NORTH boxes" p))
              #f])]
          [else
           ;(println (list "moving box NORTH no-blocker" p))
           (set-remove! boxes p)
           (set-add! boxes next)
           #t])))) 
  (define (move-^)
    ;(displayln "Move ^:")
    (match-let ([(point x y) robot])
      (let ([next (point-N robot)])
        (cond
          [(set-member? walls next)
           ;(println (list "can't move robot NORTH" robot))
           #f]
          [(set-member? boxes next)
           (cond
             [(move-box-^? next)
              ;(println (list "moving box and robot NORTH" robot))
              (set! robot next)]
             [else
              ;(println (list "can't move robot NORTH" robot))
              #f])]
          [else
           (set! robot next)]))))
  
  ;; EAST
  (define (move-box->? p)
    (match-let ([(point x y) p])
      (let ([next (point-E p)])
        (cond
          [(set-member? walls next)
           ;(println (list "can't move EAST wall" p))
           #f]
          [(set-member? boxes next)
           (cond
             [(move-box->? next)
              ;(println (list "moving box EAST blocker" p))
              (set-remove! boxes p)
              (set-add! boxes next)
              #t]
             [else
              ;(println (list "can't move EAST boxes" p))
              #f])]
          [else
           ;(println (list "moving box EAST no-blocker" p))
           (set-remove! boxes p)
           (set-add! boxes next)
           #t])))) 
  (define (move->)
    ;(displayln "Move >:")
    (match-let ([(point x y) robot])
      (let ([next (point-E robot)])
        (cond
          [(set-member? walls next)
           ;(println (list "can't move robot EAST" robot))
           #f]
          [(set-member? boxes next)
           (cond
             [(move-box->? next)
              ;(println (list "moving box and robot EAST" robot))
              (set! robot next)]
             [else
              ;(println (list "can't move robot EAST" robot))
              #f])]
          [else
           (set! robot next)]))))
  
  ;; SOUTH
  (define (move-box-v? p)
    (match-let ([(point x y) p])
      (let ([next (point-S p)])
        (cond
          [(set-member? walls next)
           ;(println (list "can't move SOUTH wall" p))
           #f]
          [(set-member? boxes next)
           (cond
             [(move-box-v? next)
              ;(println (list "moving box SOUTH blocker" p))
              (set-remove! boxes p)
              (set-add! boxes next)
              #t]
             [else
              ;(println (list "can't move SOUTH boxes" p))
              #f])]
          [else
           ;(println (list "moving box SOUTH no-blocker" p))
           (set-remove! boxes p)
           (set-add! boxes next)
           #t])))) 
  (define (move-v)
    ;(displayln "Move v:")
    (match-let ([(point x y) robot])
      (let ([next(point x (add1 y))])
        (cond
          [(set-member? walls next)
           ;(println (list "can't move robot SOUTH" robot))
           #f]
          [(set-member? boxes next)
           (cond
             [(move-box-v? next)
              ;(println (list "moving box and robot SOUTH" robot))
              (set! robot next)]
             [else
              ;(println (list "can't move robot SOUTH" robot))
              #f])]
          [else
           (set! robot next)]))))
  
  ;; WEST
  (define (move-box-<? p)
    (match-let ([(point x y) p])
      (let ([next (point-W p)])
        (cond
          [(set-member? walls next)
           ;(println (list "can't move WEST wall" p))
           #f]
          [(set-member? boxes next)
           (cond
             [(move-box-<? next)
              ;(println (list "moving box WEST blocker" p))
              (set-remove! boxes p)
              (set-add! boxes next)
              #t]
             [else
              ;(println (list "can't move WEST boxes" p))
              #f])]
          [else
           ;(println (list "moving box WEST no-blocker" p))
           (set-remove! boxes p)
           (set-add! boxes next)
           #t])))) 
  (define (move-<)
    ;(displayln "Move <:")
    (match-let ([(point x y) robot])
      (let ([next (point-W robot)])
        (cond
          [(set-member? walls next)
           ;(println (list "can't move robot WEST" robot))
           #f]
          [(set-member? boxes next)
           (cond
             [(move-box-<? next)
              ;(println (list "moving box and robot WEST" robot))
              (set! robot next)]
             [else
              ;(println (list "can't move robot WEST" robot))
              #f])]
          [else
           (set! robot next)]))))
  
  (define (move d)
    (match d
      [#\^ (move-^)]
      [#\> (move->)]
      [#\v (move-v)]
      [#\< (move-<)]
      [_ 'error]))
  (define (print-warehouse)
    (for ([row (string-split warehouse)][y (in-naturals)])
      (for ([c row][x (in-naturals)])
        (cond
          [(char=? #\# c) (display #\#)]
          [(set-member? boxes (point x y))
           (display #\O)]
          [(equal? robot (point x y)) (display #\@)]
          [else (display #\.)]))
      (displayln ""))
    (displayln ""))
  ;(print-warehouse)
  (for ([c instructions])
    (move c))
  ;(print-warehouse)
  (for/sum ([box (set->list boxes)])
    (match-let ([(point x y) box])
      (+ (* 100 y) x))))
  
(part-A test0)
(part-A test)
(part-A data)

;;

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