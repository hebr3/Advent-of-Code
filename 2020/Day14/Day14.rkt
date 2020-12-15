#lang racket
(require threading)
(require racket/match)
(require rackunit)

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
(struct memory [reg val] #:transparent)

;; Hash
(define mask (make-hash))
(define registers (make-hash))

;; Functions
(define (mask? str) (string-contains? str "mask"))
(define (mem? str) (string-contains? str "mem"))

(define (string->memory str)
  (match-let ([(list (list _ reg val)) (regexp-match* #rx"mem\\[(.+)\\] = (.+)" str #:match-select values)])
    (memory (string->number reg) (string->number val))))

(define (parse-line l)
  (cond
    [(mask? l) (update-mask l)]
    [(mem? l) (update-memory (string->memory l) (hash-ref mask 0))]))

(define (update-mask str)
  (hash-set! mask 0 (substring str 7)))

(define (update-char v m)
  (if (char=? #\X m)
      v
      m))

(define (update-val val mask)
  (~>> (for/list ([v (~r val #:base 2 #:min-width 36 #:pad-string "0")]
                  [m mask])
        (update-char v m))
      (apply string)))

(define (update-memory mem mask)
  (match-let ([(memory reg val) mem])
    (hash-set! registers reg (update-val val mask))))

(define (string->number* str)
  (string->number str 2))

;; Data
(define data
  (~>> "input.txt"
       file->list-of-strings))

(define test
  (~>> "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
"
       test->list-of-strings))

;; Puzzle
(display "test 1: ")
;(~>> test
;     (map parse-line))
;(~>> registers
;     hash->list
;     (map cdr)
;     (map string->number*)
;     (apply +))

(display "one: ")
(~>> data
     (map parse-line))
(~>> registers
     hash->list
     (map cdr)
     (map string->number*)
     (apply +))

;(display "test 2: ")
;(~>> test)
;
;(display "two: ")
;(~>> data)

;; Unit Test
