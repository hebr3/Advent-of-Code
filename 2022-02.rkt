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

(define (score-hand h)
  (cond
    [(string=? "A X" h) (+ 1 3)]
    [(string=? "A Y" h) (+ 2 6)]
    [(string=? "A Z" h) (+ 3 0)]
    
    [(string=? "B X" h) (+ 1 0)]
    [(string=? "B Y" h) (+ 2 3)]
    [(string=? "B Z" h) (+ 3 6)]

    [(string=? "C X" h) (+ 1 6)]
    [(string=? "C Y" h) (+ 2 0)]
    [(string=? "C Z" h) (+ 3 3)]))

(define (part-A L)
  (~>> L
      (map score-hand)
      (apply +)))

(part-A test)
(part-A data)

(define (score-hand-2 h)
  (cond
    [(string=? "A X" h) (+ 3 0)]
    [(string=? "A Y" h) (+ 1 3)]
    [(string=? "A Z" h) (+ 2 6)]
    
    [(string=? "B X" h) (+ 1 0)]
    [(string=? "B Y" h) (+ 2 3)]
    [(string=? "B Z" h) (+ 3 6)]

    [(string=? "C X" h) (+ 2 0)]
    [(string=? "C Y" h) (+ 3 3)]
    [(string=? "C Z" h) (+ 1 6)]))

(define (part-B L)
  (~>> L
      (map score-hand-2)
      (apply +)))

(part-B test)
(part-B data)
