#lang racket
(require threading)
(require racket/match)
(require rackunit)

(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)))

;; Structs
(struct op [type val] #:transparent)
;; Functions
(define (parse str)
  (match-let ([(list t v) (string-split str " ")])
    (op t (string->number v))))

(define (swap o)
  (match-let ([(op t v) o])
    (cond
      [(string=? "nop" t)
       (op "jmp" v)]
      [(string=? "jmp" t)
       (op "nop" v)]
      [else
       o])))

(define (swap-at idx L)
  (for/list ([i (length L)])
    (if (= i idx)
        (swap (list-ref L i))
        (list-ref L i))))

(define (run list-of-ops)
  (define (iter idx list-of-idx acc)
    (if (>= idx (length list-of-ops))
        (displayln (list "---> done" idx acc))
        (match-let ([(op t v) (list-ref list-of-ops idx)])
;          (displayln (list t v idx acc))
          (cond
            [(member idx list-of-idx)
;             (displayln (list t v idx acc))]
             '()]
            [(string=? "nop" t)
             (iter (add1 idx) (cons idx list-of-idx) acc)]
            [(string=? "acc" t)
             (iter (add1 idx) (cons idx list-of-idx) (+ acc v))]
            [(string=? "jmp" t)
             (iter (+ idx v) (cons idx list-of-idx) acc)]))))
    (iter 0 '() 0))

;; Test
(define test
  (~> "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
      (string-split _ "\n")))

(display "test 1: ")
(~> test
    (map parse _)
    run)

;; Puzzle
(define data
  (~> "input.txt"
      file->list-of-strings
      (string-split _ "\n")))

(display "one: ")
(~> data
    (map parse _)
    run)

(display "test2: ")
(for ([i (length (map parse test))])
  (run (swap-at i (map parse test))))

(display "two: ")
(for ([i (length (map parse data))])
  (run (swap-at i (map parse data))))


