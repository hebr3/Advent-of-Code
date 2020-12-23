#lang racket
(require threading)
(require racket/match)
(require rackunit)

;; Prep
(define (file->list-of-strings filename)
  (~>> filename
       open-input-file
       (read-line _ 'return-linefeed)
       (string-split _ "\n\n")
       (map (λ (x) (string-split x "\n")))
       (map rest)))

(define (test->list-of-strings str)
  (~>> str
       (string-split _ "\n\n")
       (map (λ (x) (string-split x "\n")))
       (map rest)))

;; Struct
(struct hands [h1 h2] #:transparent)

;; Functions
(define (list-of-numbers->hands list-of-numbers)
  (apply hands
         (~>> list-of-numbers
              (map (λ (x) (map string->number x))))))

(define (play-combat HANDS)
  (match-define (hands h1 h2) HANDS)
  (cond
    [(empty? h1) HANDS]
    [(empty? h2) HANDS]
    [else
     (let ([p1 (first h1)]
           [p2 (first h2)])
       (if (< p1 p2)
           (play-combat (hands (rest h1) (flatten (list (rest h2) (list p2 p1)))))
           (play-combat (hands (flatten (list (rest h1) (list p1 p2))) (rest h2)))))]))

(define (play-recursive-combat HANDS)
  (define seen (mutable-set))
  (define (iter HANDS seen)
    (match-define (hands h1 h2) HANDS)
    (cond
      [(empty? h1) HANDS]
      [(empty? h2) HANDS]
      [(set-member? seen (cons (first h1) (first h2)))
       (
      [else
       (let ([p1 (first h1)]
             [p2 (first h2)])
         (cond
           [(set-member? seen (cons p1 p2))
            (
       
  (iter HANDS seen))

(define (score-game HANDS)
  (match-let ([(hands h1 h2) HANDS])
    (let ([list-of-numbers (if (empty? h2) h1 h2)])
      (for/sum ([i list-of-numbers]
                [j (range (length list-of-numbers) 0 -1)])
        (* i j)))))

;; Data
(define data
  (~>> "input/2020-22.txt"
       file->list-of-strings))

(define test
  (~>> "Player 1:
43
19

Player 2:
2
29
14"
       test->list-of-strings))


;; Puzzle
;(display "test 1: ")
;(~>> test
;     list-of-numbers->hands
;     play-combat
;     score-game)
;
;(display "one: ")
;(~>> data
;     list-of-numbers->hands
;     play-combat
;     score-game)
;
(display "test 2: ")
(~>> test
     list-of-numbers->hands
     play-recursive-combat
     score-game)
;
(display "two: ")
(~>> data
     list-of-numbers->hands
     play-recursive-combat)

;; Unit Test
