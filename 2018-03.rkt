#lang racket

(struct pt [x y] #:transparent)

(define test1
  (list "1 @ 1,3: 4x4"
        "2 @ 3,1: 4x4"
        "3 @ 5,5: 2x2"))

(define rx #rx"@ (.+),(.+): (.+)x(.+)")

(define (string->locs str)
  (match-let ([(list m X Y DX DY) (regexp-match rx str)])
    (for*/list ([dx (string->number DX)]
                [dy (string->number DY)])
      (pt (+ 1 (string->number X) dx)
          (+ 1 (string->number Y) dy)))))

(count (λ (x) (< 1 (length x)))
       (group-by (λ (x) x)
                 (flatten (map string->locs test1))))

(define input
  (file->lines "input.txt"))

(count (λ (x) (< 1 (length x)))
       (group-by (λ (x) x)
                 (flatten (map string->locs input))))

(define total2
  (flatten (map string->locs input)))

(- (length total2)
   (set-count (list->set total2)))
