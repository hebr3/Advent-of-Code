#lang racket
(require threading)
(require racket/match)
(require lens)
(require pmap)

(define data
  (~> "input/2021-25.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")))

(define test1
  '("...>..."
    "......."
    "......>"
    "v.....>"
    "......>"
    "......."
    "..vvv.."))

(define test2
  '("v...>>.vv>"
    ".vv>>.vv.."
    ">>.>v>...v"
    ">>v>>.>.v."
    "v>v.vv.v.."
    ">.>>..v..."
    ".vv..>.>v."
    "v.v..>>v.v"
    "....v..v.>"))

(struct Board [width height rights downs] #:transparent)

(define (make-entry x y)
  (format "~a ~a" x y))
(define (sort* LoS)
  (sort LoS string<?))

(define (format-data data)
  (define rights '())
  (define downs '())
  (let ([height (length data)]
        [width (string-length (car data))])
    (for ([y height])
      (for ([x width])
        (let ([c-char (string-ref (list-ref data y) x)]
              [c-val (make-entry x y)])
          (when (char=? c-char #\v)
            (set! downs (cons c-val downs)))
          (when (char=? c-char #\>)
            (set! rights (cons c-val rights))))))
    (Board width height (sort* rights) (sort* downs))))

(define (update board)
  (define (iter board* cnt)
    (define next-rs '())
    (define next-ds '())
    (match-let ([(Board w h rs ds) board*])
      (for ([r rs])
        (match-let ([(list x y) (string-split r)])
          (let ([next-r (make-entry (modulo (add1 (string->number x)) w) y)])
            (if (or (member next-r rs)
                    (member next-r ds))
                (set! next-rs (cons r next-rs))
                (set! next-rs (cons next-r next-rs))))))
      (for ([d ds])
        (match-let ([(list x y) (string-split d)])
          (let ([next-d (make-entry x (modulo (add1 (string->number y)) h))])
            (if (or (member next-d next-rs)
                    (member next-d ds))
                (set! next-ds (cons d next-ds))
                (set! next-ds (cons next-d next-ds))))))
      (displayln cnt)
      (if (and (equal? (sort* next-rs) rs)
               (equal? (sort* next-ds) ds))
          cnt
          (iter (Board w h (sort* next-rs) (sort* next-ds)) (add1 cnt)))))    
  (iter board 1))
    
(~> test2
    format-data
    update)

(~> data
    format-data
    update)
