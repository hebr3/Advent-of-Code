#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2021-13.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")))

(define test
  '("6,10"
    "0,14"
    "9,10"
    "0,3"
    "10,4"
    "4,11"
    "6,0"
    "6,12"
    "4,1"
    "0,13"
    "10,12"
    "3,4"
    "3,0"
    "8,4"
    "1,10"
    "2,14"
    "8,10"
    "9,0"
    ""
    "fold along y=7"
    "fold along x=5"))

(define (string->point str)
  (match-let ([(list x y) (string-split str ",")])
    (list (string->number x) (string->number y))))

(define (string->fold str)
  (match-let ([(list a b c) (string-split str " ")])
    (match-let ([(list dir val) (string-split c "=")])
      (list dir (string->number val)))))

(define (format-data LoS)
  (let ([idx (index-of LoS "")])
    (let ([points (for/list ([i idx]) (string->point (list-ref LoS i)))]
          [folds (for/list ([i (sub1 (- (length LoS) idx))]) (string->fold (list-ref LoS (add1 (+ idx i)))))])
      (list points folds))))

(define (fold-list points flips)
  (let ([tmp-p points]
        [tmp-f flips])
    (for ([i (length flips)])
      (match-let ([(list dir val) (car tmp-f)])
        (define (vertical-flip pt val)
          (match-let ([(list x y) pt])
            (if (< val y)
                (list x (- val (- y val)))
                pt)))
        (define (horizontal-flip pt val)
          (match-let ([(list x y) pt])
            (if (< val x)
                (list (- val (- x val)) y)
                pt)))
        (set! tmp-p (for/list ([pt tmp-p])
                      (if (string=? "y" dir)
                          (vertical-flip pt val)
                          (horizontal-flip pt val))))
        (set! tmp-f (cdr tmp-f))))
    (remove-duplicates tmp-p)))

(define (print-points pts)
  (let ([max-x (add1 (apply max (map car pts)))]
        [max-y (add1 (apply max (map cadr pts)))])
    (for ([y max-y])
      (for ([x max-x])
        (if (member (list x y) pts)
            (display "0")
            (display " ")))
      (displayln ""))))

(~> data
    format-data
    (apply fold-list _)
    print-points
    )
