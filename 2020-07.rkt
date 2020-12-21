#lang racket
(require threading)
(require racket/match)


(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)
      (string-split _ "\n")))

;; Function
(define (format-row str)
  (string-split str #px" bags contain \\d | bags, \\d | bag, \\d | bag.| bags."))

(define (graph-it L)
  (for/list ([i (rest L)])
    (string-join (list (first L) "->" i))))

;; Test
(define test
  (~> "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
red test bags contain 1 light red bag."
      (string-split _ "\n")
      (map format-row _)
      (map (λ (x) (map (λ (y) (string-replace y " " "_")) x)) _)))

(define test-bags (make-hash))

(display "test 1: ")
(~>> test
     (map graph-it _)
     flatten)

;; Puzzle
(define data
  (~>> "input.txt"
       file->list-of-strings
       (map format-row _)
       (map (λ (x) (map (λ (y) (string-replace y " " "_")) x)) _)))

(define data-bags (make-hash))
;(define data-bags '("shiny gold"))

(display "one: ")
(~>> data
     (map graph-it _)
     flatten)

;(display "test 2: ")
;(~> test)
;
;(display "two: ")
;(~> data)
