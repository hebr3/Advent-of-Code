#lang racket
(require threading)
(require racket/match)
(require rackunit)

;; Prep
(define (file->vector-of-char filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)
      (string-split "\n")))

(define (test->vector-of-char str)
  (~> str
      (string-split "\n")))

;; Struct

;; Functions
(define (get-buses str)
  (~> str
      (string-split ",")
      (filter (λ (x) (not (string=? x "x"))) _)
      (map string->number _)))

(define (convert-input input)
  (match-let ([(list tme buses) input])
    (list (string->number tme) (get-buses buses))))

(define (nearest-future future-date bus-time)
  (~> future-date
      string->number
      (/ bus-time)
      ceiling
      (* bus-time)
      (cons bus-time)))

(define (calculate-future-buses future-time list-of-buses)
  (map (λ (bus) (nearest-future future-time bus)) list-of-buses))

(define (find-solution future-time nearest)
  (match-let ([(cons nearest-time bus-id) nearest])
    (* (- nearest-time (string->number future-time))
       bus-id)))

(define (future-buses input)
  (match-let ([(list future-time buses) input])
    (~>> buses
        get-buses
        (calculate-future-buses future-time)
        (sort _ < #:key car)
        first
        (find-solution future-time)
        )))

;; Data
(define data
  (~>> "input.txt"
       file->vector-of-char))

(define test
  (~>> "939
7,13,x,x,59,x,31,19
"
       test->vector-of-char))


;; Puzzle
(display "test 1: ")
(~>> test
     future-buses)

(display "one: ")
(~>> data
     future-buses)

;(display "test 2: ")
;(~>> test)
;
;(display "two: ")
;(~>> data)

;; Unit Test
