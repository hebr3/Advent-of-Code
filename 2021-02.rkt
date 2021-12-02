#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2021-02.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")))


(define test
  '("forward 5"
    "down 5"
    "forward 8"
    "up 3"
    "down 8"
    "forward 2"))

(define (read-instruction inst pt)
  (match-let ([(list dir val) (string-split inst)]
              [(list m n) pt])
    (cond
      [(string=? "forward" dir)
       (list (+ m (string->number val)) n)]
      [(string=? "down" dir)
       (list m (+ n (string->number val)))]
      [(string=? "up" dir)
       (list m (- n (string->number val)))])))

(apply * 
(foldl read-instruction '(0 0) test))

(apply * 
(foldl read-instruction '(0 0) data))

(define (read-instruction2 inst pt)
  (match-let ([(list dir val) (string-split inst)]
              [(list horizontal aim depth) pt])
    (cond
      [(string=? "forward" dir)
       (list (+ horizontal (string->number val)) aim (+ depth (* aim (string->number val))))]
      [(string=? "down" dir)
       (list horizontal (+ aim (string->number val)) depth)]
      [(string=? "up" dir)
       (list horizontal (- aim (string->number val)) depth)])))

(match-let ([(list x y z) (foldl read-instruction2 '(0 0 0) test)])
  (* x z))

(match-let ([(list x y z) (foldl read-instruction2 '(0 0 0) data)])
  (* x z))