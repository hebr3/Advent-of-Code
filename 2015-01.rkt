#lang racket
(require threading)
(require racket/match)
(require rackunit)

(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)))

;; Structs

;; Functions
(define (char->number char)
  (cond
    [(char=? #\( char) 1]
    [(char=? #\) char) -1]
    [else 0]))

(define (find-negative list-of-numbers)
  (define (iter val acc L)
    (cond
      [(empty? L) acc]
      [else
       (iter (+ val (first L)) (cons val acc) (rest L))]))
  (~> (iter 0 '() list-of-numbers)
      reverse
;      (takef (λ (x) (<= 0 x)))
;      length))
      (index-of _ -1)))

;; Test
(define test
  (~> "(())
()()
(((
(()(()(
))(((((
())
))(
)))
)())())"
      (string-split _ "\n")))

(display "test 1: ")
(~>> test
     (map string->list)
     (map (λ (x) (map (λ (xx) (char->number xx)) x)))
     (map (λ (x) (apply + x))))

;; Puzzle
(define data
  (~>> "input"
       (file->list-of-strings)))

(display "one: ")
(~>> data
    string->list
    (map (λ (x) (char->number x)))
    (apply +))

;(display "test2: ")

(display "two: ")
(~>> data
     string->list
     (map (λ (x) (char->number x)))
     find-negative)

