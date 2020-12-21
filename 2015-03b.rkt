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
(define (update-location location direction)
  (match-let ([(list x y) location])
    (cond
      [(char=? #\^ direction) (list x (add1 y))]
      [(char=? #\v direction) (list x (sub1 y))]
      [(char=? #\> direction) (list (add1 x) y)]
      [(char=? #\< direction) (list (sub1 x) y)])))

(define (read-directions current-location list-of-char)
  (cond
    [(empty? list-of-char) (list current-location)]
    [else
     (let ([next-location (update-location current-location (car list-of-char))])
       (cons current-location
             (read-directions next-location (rest list-of-char))))]))

(define (find-unique-houses list-of-char)
  (~>> list-of-char
       (read-directions (list 0 0))
       (apply set)))

(define (split-list list-of-char)
  (set-union (find-unique-houses (for/list ([i (length list-of-char)] #:when (even? i)) (list-ref list-of-char i)))
             (find-unique-houses (for/list ([i (length list-of-char)] #:when (odd? i)) (list-ref list-of-char i)))))

;; Data
(define data
  (~>> "input.txt"
       (file->list-of-strings)
       (string-split _ "\n")))

;; Test
(define test
  (~> ">
^>v<
^v^v^v^v^v
"
      (string-split _ "\n")))

;; Puzzle

(display "test 1: ")
(~>> test
     (map string->list)
     (map (λ (x) (read-directions (list 0 0) x)))
     (map (λ (x) (apply set x)))
     (map set-count))

(display "one: ")
(~>> data
     first
     string->list
     find-unique-houses
     set-count)

(display "test2: ")
(~>> test
     (map string->list)
     (map (λ (x) (split-list x)))
;     (map set-count))
)
(display "two: ")
(~>> data
     first
     string->list
     split-list
     set-count)

