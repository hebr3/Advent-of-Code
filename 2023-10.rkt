#lang racket
(require threading)
(require racket/match)
(require "util.rkt")

(define data (input->data "input/2023-10.txt"))

(define test1 "-L|F7
7S-7|
L|7||
-L-J|
L|-JF")

(define test2 "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
")

(define test3 "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........")

(define test3b "..........
.S------7.
.|F----7|.
.||OOOO||.
.||OOOO||.
.|L-7F-J|.
.|II||II|.
.L--JL--J.
..........")

(define test4 ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")

(define test5 "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")

;;

(struct point [x y] #:transparent)

(define PIPE-CHAR (string->list "S|-LJ7F"))
(define OPEN-DW (string->list "S|7F"))
(define OPEN-UP (string->list "S|JL"))
(define OPEN-LT (string->list "S-J7"))
(define OPEN-RT (string->list "S-LF"))

(define (DFS adj-hash start)
  (define (iter stack acc)
    (cond
      [(empty? stack) acc]
      [else
       (let* ([current (first stack)]
              [rest-stack (rest stack)]
              [neighbors (hash-ref adj-hash current)]
              [valid-neigh (filter (λ (x) (not (member x acc))) neighbors)]
              [new-stack (flatten (cons valid-neigh rest-stack))])
         (iter new-stack (cons current acc)))]))
  (iter (list start) '()))

(define (part-A L)
  (define PIPES (make-hash))
  (define rows (string-split L "\n"))
  (for ([row rows][y (in-naturals)])
    (for ([c row][x (in-naturals)])
      (when (member c PIPE-CHAR)
        (hash-set! PIPES (point x y) '()))))
  
  (define (rows-ref pt)
    (match-let ([(point x y) pt])
      (string-ref (list-ref rows y) x)))
  
  (define (matched-vert? p-up p-dw)
    (and (hash-has-key? PIPES p-up)
         (hash-has-key? PIPES p-dw)
         (member (rows-ref p-up) OPEN-DW)
         (member (rows-ref p-dw) OPEN-UP)))
  (define (matched-hort? p-lt p-rt)
    (and (hash-has-key? PIPES p-lt)
         (hash-has-key? PIPES p-rt)
         (member (rows-ref p-lt) OPEN-RT)
         (member (rows-ref p-rt) OPEN-LT)))

  (define (hash-update! p1 p2)
    (hash-set! PIPES p1 (cons p2 (hash-ref PIPES p1))))

  (define START (point 0 0))
  (for* ([y (length rows)]
         [x (sub1 (string-length (first rows)))])
    (let ([pt (point x y)]
          [p-dw (point x (add1 y))]
          [p-rt (point (add1 x) y)])
      (when (matched-vert? pt p-dw)
        (hash-update! pt p-dw)
        (hash-update! p-dw pt))
      (when (matched-hort? pt p-rt)
        (hash-update! pt p-rt)
        (hash-update! p-rt pt))
      (when (char=? #\S (rows-ref pt))
        (set! START pt))))

  (define (half lst)
    (floor (/ (length lst) 2)))

  (define (remap-char c)
    (cond
      [(char=? #\F c) "\u250c"]
      [(char=? #\7 c) "\u2510"]
      [(char=? #\L c) "\u2514"]
      [(char=? #\J c) "\u2518"]
      [(char=? #\- c) "\u2500"]
      [(char=? #\| c) "\u2502"]
      [(char=? #\S c) "\u253c"]))
      
  
  (define LOOP (DFS PIPES START))
  (for ([row rows][y (in-naturals)])
    (for ([c row][x (in-naturals)])
      (if (member (point x y) LOOP)
          (display (remap-char c))
          (display c)))
    (displayln ""))
  (half LOOP))

  
;; (part-A test1)
;; (part-A test2)
;; (part-A test3)
;; (part-A test4)
;; (part-A test5)
;;(part-A data)

;;

(define (part-B L)
  (define PIPES (make-hash))
  (define rows (string-split L "\n"))
  (for ([row rows][y (in-naturals)])
    (for ([c row][x (in-naturals)])
      (when (member c PIPE-CHAR)
        (hash-set! PIPES (point x y) '()))))
  
  (define (rows-ref pt)
    (match-let ([(point x y) pt])
      (string-ref (list-ref rows y) x)))
  
  (define (matched-vert? p-up p-dw)
    (and (hash-has-key? PIPES p-up)
         (hash-has-key? PIPES p-dw)
         (member (rows-ref p-up) OPEN-DW)
         (member (rows-ref p-dw) OPEN-UP)))
  (define (matched-hort? p-lt p-rt)
    (and (hash-has-key? PIPES p-lt)
         (hash-has-key? PIPES p-rt)
         (member (rows-ref p-lt) OPEN-RT)
         (member (rows-ref p-rt) OPEN-LT)))

  (define (hash-update! p1 p2)
    (hash-set! PIPES p1 (cons p2 (hash-ref PIPES p1))))

  (define START (point 0 0))
  (for* ([y (length rows)]
         [x (sub1 (string-length (first rows)))])
    (let ([pt (point x y)]
          [p-rt (point (add1 x) y)])
      (when (matched-hort? pt p-rt)
        (hash-update! pt p-rt)
        (hash-update! p-rt pt))))
  (for* ([y (sub1 (length rows))]
         [x (string-length (first rows))])
    (let ([pt (point x y)]
          [p-dw (point x (add1 y))])
      (when (matched-vert? pt p-dw)
        (hash-update! pt p-dw)
        (hash-update! p-dw pt))))
  (for* ([y (length rows)]
         [x (string-length (first rows))])
    (let ([pt (point x y)])
      (when (char=? #\S (rows-ref pt))
        (set! START pt))))

  (define (half lst)
    (floor (/ (length lst) 2)))

  (define (remap-char c)
    (cond
      [(char=? #\F c) "\u250c"]
      [(char=? #\7 c) "\u2510"]
      [(char=? #\L c) "\u2514"]
      [(char=? #\J c) "\u2518"]
      [(char=? #\- c) "\u2500"]
      [(char=? #\| c) "\u2502"]
      [(char=? #\S c) "\u253c"]))
  
  (define LOOP (DFS PIPES START))

  (for ([row rows][y (in-naturals)])
    (for ([c row][x (in-naturals)])
      (if (member (point x y) LOOP)
          (display (remap-char c))
          (display #\o)))
    (displayln ""))

  (for*/sum ([y (length rows)][x (string-length (first rows))]
                          #:do [(define pt (point x y))]
                          #:when (not (member pt LOOP)))
    (if (odd? (for/sum ([i (range x (string-length (first rows)))])
      (if (and (member (point (add1 i) (add1 y)) LOOP)
               (member (point (add1 i) y) LOOP)
               (member (point (add1 i) (add1 y)) (hash-ref PIPES (point (add1 i) y))))
          1 0) 
      )) 1 0))
  
;;   (length (filter odd?
;;           (for*/list ([y (length rows)]
;;               [x (string-length (first rows))]
;;               #:do [(define pt (point x y))]
;;               #:when (not (member pt LOOP)))
;;     (for*/sum ([i y])
;;       (println (list 'pt (point x y)
;;             'up (point x (- y i 1))
;;             'u-r (if (member (point x (- y i 1)) LOOP) #t #f)
;;             'di (point (add1 x) (- y i 1))
;;             'd-r (if (member (point (add1 x) (- y i 1)) LOOP) #t #f)
;;             'both (and (member (point x (- y i 1)) LOOP)
;;                        (member (point (add1 x) (- y i 1)) LOOP)
;;                        #t)
;;                       
;;             ))
;;       (or (and (member (point x (- y i 1)) LOOP)
;;                (member (point (add1 x) (- y i 1)) LOOP)
;;                1)
;;           0)
;;       ))))
  )
          
(part-B test1)
(part-B test2)
(part-B test3)
(part-B test3b)
(part-B test4)
(part-B test5)
(part-B data)
