#lang racket
(require threading)
(require racket/match)
(require rackunit)
(require racket/set)

;; Prep
(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)
      (string-split "\n")))

(define (test->list-of-strings str)
  (~> str
      (string-split "\n")))

;; Struct

;; Functions
(define (parse-input loloc)
  (define S (mutable-set))
  (for* ([l (length loloc)]
         [s (length (first loloc))])
    (when (char=? #\# (list-ref (list-ref loloc s) l))
        (set-add! S (list l s 0 0))))
  S)



(define (count-active-neighbors cell SET)
  (match-let ([(list X Y Z W) cell]) 
    (for*/sum ([x '(-1 0 1)]
               [y '(-1 0 1)]
               [z '(-1 0 1)]
               [w '(-1 0 1)]
               #:when (not (= 0 x y z w)))
      (if (set-member? SET (list (+ X x) (+ Y y) (+ Z z) (+ W w)))
          1 0))))

(define (check-neighbors a SET S)
  (match-let ([(list X Y Z W) a])
    (for* ([x '(-1 0 1)][y '(-1 0 1)][z '(-1 0 1)][w '(-1 0 1)])
      (let ([cell (list (+ X x) (+ Y y) (+ Z z) (+ W w))])
        (cond
          [(and (set-member? SET cell)
                (<= 2 (count-active-neighbors cell SET) 3))
           (set-add! S cell)]
          [(and (not (set-member? SET cell))
                (= 3 (count-active-neighbors cell SET)))
           (set-add! S cell)])))))

(define (next-cycle SET)
  (define S (mutable-set))
  (define active (set->list SET))
  (for ([a active])
    (check-neighbors a SET S))
  S)

;; Data
(define data
  (~>> "input.txt"
       file->list-of-strings
       (map string->list)))

(define test
  (~>> ".#.
..#
###
"
       test->list-of-strings
       (map string->list)
       ))


;; Puzzle
(display "test 1: ")
(~>> test
     parse-input
     next-cycle
     next-cycle
     next-cycle
     next-cycle
     next-cycle
     next-cycle
     set->list
     length)

(display "one: ")
(~>> data
     parse-input
     next-cycle
     next-cycle
     next-cycle
     next-cycle
     next-cycle
     next-cycle
     set->list
     length)

;(display "test 2: ")
;(~>> test)
;
;(display "two: ")
;(~>> data)

;; Unit Test
