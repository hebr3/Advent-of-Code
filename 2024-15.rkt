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
  (define (point-N p)
    (match-let ([(point x y) p])
      (point x (sub1 y))))
  (define (point-E p)
    (match-let ([(point x y) p])
      (point (add1 x) y)))
  (define (point-S p)
    (match-let ([(point x y) p])
      (point x (add1 y))))
  (define (point-W p)
    (match-let ([(point x y) p])
      (point (sub1 x) y)))
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
(define (part-B L X-MAX Y-MAX)
  L)
;(part-B test)
;(part-B data)