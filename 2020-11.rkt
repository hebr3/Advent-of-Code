#lang racket
(require threading)
(require racket/match)
(require rackunit)

;; Prep
(define (file->vector-of-char filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)
      (string-split "\n")
      (map (λ (x) (string->list x)) _)
      (map (λ (x) (list->vector x)) _)
      list->vector))

(define (test->vector-of-char str)
  (~> str
      (string-split "\n")
      (map (λ (x) (string->list x)) _)
      (map (λ (x) (list->vector x)) _)
      list->vector))

;; Functions
(define (vector-ref-xy vec x y)
  (vector-ref (vector-ref vec y) x))

(define (get-dimensions vec)
  (list (vector-length (vector-ref vec 0)) (vector-length vec)))

(define (count-neighbors vec x y)
  (match-let ([(list X Y) (get-dimensions vec)])
    (for*/sum ([i '(-1 0 1)]
               [j '(-1 0 1)]
               #:when (not (= 0 i j)))
      (let ([x* (+ x i)][y* (+ y j)])
        (cond
          [(and (<= 0 x* (sub1 X))
                (<= 0 y* (sub1 Y))
                (char=? #\# (vector-ref-xy vec x* y*)))
           1]
          [else 0])))))

(define (update-seat vec x y)
  (define (floor? c) (char=? #\. c))
  (define (occupied-seat? c) (char=? #\# c))
  (define (empty-seat? c) (char=? #\L c))
  (match-let* ([(list X Y) (get-dimensions vec)]
               [S (vector-ref-xy vec x y)]
               [C (count-neighbors vec x y)])
    (cond
      [(floor? S) #\.]
      [(and (occupied-seat? S) (< 3 C)) #\L]
      [(occupied-seat? S) #\#]
      [(zero? C) #\#]
      [else #\L])))

(define (update-seats vec)
  (match-let ([(list X Y) (get-dimensions vec)])
    (for/vector ([y Y])
      (for/vector ([x X])
        (update-seat vec x y)))))

(define (count-occupied-seats vec)
  (define (occupied-seat? c) (char=? #\# c))
  (for/sum ([i vec])
    (vector-count occupied-seat? i)))

(define (find-fixed-point-for-seats vec C)
  (let ([C* (count-occupied-seats vec)])
    (cond
      [(= C C*) C]
      [else (find-fixed-point-for-seats (update-seats vec) C*)])))

(define (display-seats vec)
  (for ([i  (~>> vec
                 vector->list
                 (map (λ (x) (apply string (vector->list x)))))])
    (displayln i)))

;; Data
(define data
  (~>> "input/2020-11.txt"
       file->vector-of-char))

(define test
  (~>> "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
"
       test->vector-of-char))

(define r1
  (~>> "#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##
"
       test->vector-of-char))

(define r2
  (~>> "#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##
"
       test->vector-of-char))

;; Puzzle
(display "test 1: ")
(~> test
    (find-fixed-point-for-seats -1))

(display "one: ")
(time
(~> data
    (find-fixed-point-for-seats -1)))

;(display "test 2: ")
;(~>> test)
;
;(display "two: ")
;(~>> data)

;; Unit Test
(check-equal? (vector-ref-xy test 1 0)
              #\.)
(check-equal? (vector-ref-xy test 1 1)
              #\L)
(check-equal? (vector-ref-xy test 1 2)
              #\.)

(check-equal? (get-dimensions test)
              '(10 10))
(check-equal? (get-dimensions data)
              '(96 91))

(check-equal? (count-neighbors test 0 0)
              0)
(check-equal? (count-neighbors r1 0 0)
              2)

(check-equal? (update-seat test 0 0)
              #\#)
(check-equal? (update-seat test 1 0)
              #\.)

(check-equal? (update-seats test)
              r1)
(check-equal? (update-seats r1)
              r2)

(check-equal? (find-fixed-point-for-seats test -1)
              37)


(check-equal? (vector-ref-xy data 0 0)
              #\L)
(check-equal? (vector-ref-xy data 0 1)
              #\.)
