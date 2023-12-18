#lang racket
(require graph)
(require data/heap)
(require (only-in "util.rkt" input->data tap point))

(define data (input->data "input/2023-18.txt"))

(define test "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)")

;;

(define (UP pt num)
  (match-let ([(point x y) pt])
    (point x (+ y num))))

(define (DOWN pt num)
  (match-let ([(point x y) pt])
    (point x (- y num))))

(define (LEFT pt num)
  (match-let ([(point x y) pt])
    (point (- x num) y)))

(define (RIGHT pt num)
  (match-let ([(point x y) pt])
    (point (+ x num) y)))

(define (parse->inst str)
  (match-let ([(list dir num _) (string-split str)])
    (list dir (string->number num))))

(define (shoelace lop)
  (let ([N (length lop)])
    (* 1/2 (abs(for/sum ([i N])
                 (match-let ([(point x1 y1) (list-ref lop i)]
                             [(point x2 y2) (list-ref lop (modulo (add1 i) N))])
                   (- (* x1 y2) (* x2 y1))))))))

(define (part-A L)
  (let* ([rows (string-split L "\n")]
         [instructions (map parse->inst rows)]
         [PERIMETER (for/sum ([pt instructions]) (second pt))]
         [POINTS (list (point 0 0))])
    (for ([inst instructions])
      (let ([head (first POINTS)])
        (match-let ([(list D num) inst])
          (match D
            ["R" (set! POINTS (cons (RIGHT head num) POINTS))]
            ["L" (set! POINTS (cons (LEFT head num) POINTS))]
            ["U" (set! POINTS (cons (UP head num) POINTS))]
            ["D" (set! POINTS (cons (DOWN head num) POINTS))]))))
    (set! POINTS (cdr POINTS))
    (+ (- (shoelace POINTS)
          (/ PERIMETER 2) -1)
       PERIMETER)))

(part-A test)
(part-A data)

;;

(define (parse->inst2 str)
  (match-let* ([(list _ hex) (string-split str #px"#|\\)")])
    (let ([num (string->number (substring hex 0 5) 16)]
          [dir (substring hex 5)])
      (list dir num))))

(define (part-B L)
  (let* ([rows (string-split L "\n")]
         [instructions (map parse->inst2 rows)]
         [PERIMETER (for/sum ([pt instructions]) (second pt))]
         [POINTS (list (point 0 0))])
    (for ([inst instructions])
      (let ([head (first POINTS)])
        (match-let ([(list D num) inst])
          (match D
            ["0" (set! POINTS (flatten (list (RIGHT head num) POINTS)))]
            ["2" (set! POINTS (flatten (list (LEFT head num) POINTS)))]
            ["3" (set! POINTS (flatten (list (UP head num) POINTS)))]
            ["1" (set! POINTS (flatten (list (DOWN head num) POINTS)))]))))
    (set! POINTS (cdr POINTS))
    (+ (- (shoelace POINTS)
          (/ PERIMETER 2) -1)
       PERIMETER)))

(part-B test)
(part-B data)