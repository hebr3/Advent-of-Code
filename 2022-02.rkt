#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2022-02.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")
      ))

(define test '("A Y"
"B X"
"C Z"))

(struct hand [oppenent player val win] #:transparent)

(define (line->hand str)
  (match-let ([(list o h) (string-split str " ")])
    (cond
      [(and (string=? "A" o) (string=? "X" h))
       (hand 'rock 'rock 1 3)]
      [(and (string=? "A" o) (string=? "Y" h))
       (hand 'rock 'paper 2 6)]
      [(and (string=? "A" o) (string=? "Z" h))
       (hand 'rock 'scissor 3 0)]
      
      [(and (string=? "B" o) (string=? "X" h))
       (hand 'paper 'rock 1 0)]
      [(and (string=? "B" o) (string=? "Y" h))
       (hand 'paper 'paper 2 3)]
      [(and (string=? "B" o) (string=? "Z" h))
       (hand 'paper 'scissor 3 6)]

      [(and (string=? "C" o) (string=? "X" h))
       (hand 'scissor 'rock 1 6)]
      [(and (string=? "C" o) (string=? "Y" h))
       (hand 'scissor 'paper 2 0)]
      [(and (string=? "C" o) (string=? "Z" h))
       (hand 'scissor 'scissor 3 3)])))

(define (score h)
  (match-let ([(hand o p v w) h])
    (+ v w)))

(define (part-A L)
  (~>> L
      (map line->hand)
      (map score)
      (apply +)))

(part-A test)
(part-A data)

(define (line->hand* str)
  (match-let ([(list o h) (string-split str " ")])
    (cond
      [(and (string=? "A" o) (string=? "X" h))
       (hand 'rock 'scissor 3 0)]
      [(and (string=? "A" o) (string=? "Y" h))
       (hand 'rock 'rock 1 3)]
      [(and (string=? "A" o) (string=? "Z" h))
       (hand 'rock 'paper 2 6)]
      
      [(and (string=? "B" o) (string=? "X" h))
       (hand 'paper 'rock 1 0)]
      [(and (string=? "B" o) (string=? "Y" h))
       (hand 'paper 'paper 2 3)]
      [(and (string=? "B" o) (string=? "Z" h))
       (hand 'paper 'scissor 3 6)]

      [(and (string=? "C" o) (string=? "X" h))
       (hand 'scissor 'paper 2 0)]
      [(and (string=? "C" o) (string=? "Y" h))
       (hand 'scissor 'scissor 3 3)]
      [(and (string=? "C" o) (string=? "Z" h))
       (hand 'scissor 'rock 1 6)])))

(define (part-B L)
    (~>> L
      (map line->hand*)
      (map score)
      (apply +)))

(part-B test)
(part-B data)
