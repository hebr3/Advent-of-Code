#lang racket
(require threading)
(require file/md5)
(require racket/match)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2023-02.txt"))

(define test "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

;;

(struct pull [red green blue] #:transparent)

(define (update-pull old-pull new-pull)
  (match-let ([(pull or og ob) old-pull]
              [(pull nr ng nb) new-pull])
    (pull (max or nr) (max og ng) (max ob nb))))

(define (read-game str)
  (define init (pull 0 0 0))
  (for ([p (rest (string-split str #px":|;"))])
    (define r (regexp-match #px"(\\d+) red" p))
    (define g (regexp-match #px"(\\d+) green" p))
    (define b (regexp-match #px"(\\d+) blue" p))
    (when r
      (set! init (update-pull init (pull (string->number (second r)) 0 0))))
    (when g
      (set! init (update-pull init (pull 0 (string->number (second g)) 0))))
    (when b
      (set! init (update-pull init (pull 0 0 (string->number (second b))))))
    ;(println (list r g b))
    )
  init)

(define (valid-pull? p)
  (match-let ([(cons _ (pull r b g)) p])
    (and (<= r 12) (<= b 13) (<= g 14))))

(define (part-A L)
  (define LoG (string-split L "\n"))
  (define GAMES (make-hash))
  (define empty-pull (pull 0 0 0))
  (for ([g LoG])
    (define id (string->number (second (regexp-match #px"(\\d+):" g))))
    (define new-pull (read-game g))
    (define old-pull (hash-ref GAMES id empty-pull))
    ;(println (list old-pull new-pull))
    (define update (update-pull old-pull new-pull))
    (hash-set! GAMES id update))
  (~>> GAMES
       hash->list
       (filter valid-pull?)
       (map car)
       (apply +)))

(part-A test)
(part-A data)

;;

(define (power p)
  (match-let ([(cons _ (pull r g b)) p])
    (* r g b)))

(define (part-B L)
  (define LoG (string-split L "\n"))
  (define GAMES (make-hash))
  (define empty-pull (pull 0 0 0))
  (for ([g LoG])
    (define id (string->number (second (regexp-match #px"(\\d+):" g))))
    (define new-pull (read-game g))
    (define old-pull (hash-ref GAMES id empty-pull))
    (println (list old-pull new-pull))
    (define update (update-pull old-pull new-pull))
    (hash-set! GAMES id update))
  (~>> GAMES
       hash->list
       (map power)
       ;(filter valid-pull?)
       ;(map car)
       (apply +)
       ))

(part-B test)
(part-B data)
