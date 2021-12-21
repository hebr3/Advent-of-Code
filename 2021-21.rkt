#lang racket
(require threading)
(require racket/match)
(require lens)


(struct player [space score] #:transparent)
(struct game [player1 player2 roll-count] #:transparent)

(define deterministic-die
  (let ([n 0])
    (λ ([reset #f])
      (if reset
          (set! n 0)
          (begin0
            (+ (* 9 n) 6)
            (set! n (add1 n)))))))

(define data
  (~> "input/2021-21.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")))

(define test
  '("Player 1 starting position: 4"
    "Player 2 starting position: 8"))

(define (format-data data)
  (define (get-loc str)
    (~> str
        (string-split _ ": ")
        second
        string->number))
  (match-let ([(list p1 p2) (map get-loc data)])
    (game (player p1 0) (player p2 0) 0)))

(define (update-player pl)
  (match-let ([num (deterministic-die)]
              [(player pl-space pl-score) pl])
    (let ([new-space (modulo (+ pl-space num) 10)])
      (when (zero? new-space)
        (set! new-space 10))
      (player new-space (+ pl-score new-space)))))

(define (update game-state)
  (match-let ([(game player-1 player-2 rolls)
               game-state])
    ;(displayln game-state)
    (if (even? rolls)
        (game (update-player player-1) player-2 (+ 3 rolls))
        (game player-1 (update-player player-2) (+ 3 rolls)))))

(define (play-until game-state)
  (match-let* ([(game player-1 player-2 rolls) game-state]
               [(player player-one-space player-one-score) player-1]
               [(player player-two-space player-two-score) player-2])
    (if (or (<= 1000 player-one-score)
            (<= 1000 player-two-score))
        game-state
        (play-until (update game-state)))))

(define (score-game game-state)
  (match-let ([(game (player _ p1-score) (player _ p2-score) rolls) game-state])
    (* rolls (min p1-score p2-score))))

(~> test
    format-data
    play-until
    score-game)

(deterministic-die #t)

(~> data
    format-data
    play-until
    score-game)