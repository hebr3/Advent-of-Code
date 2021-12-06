#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2021-05.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")))

(define test
  '("0,9 -> 5,9"
    "8,0 -> 0,8"
    "9,4 -> 3,4"
    "2,2 -> 2,1"
    "7,0 -> 7,4"
    "6,4 -> 2,0"
    "0,9 -> 2,9"
    "3,4 -> 1,4"
    "0,0 -> 8,8"
    "5,5 -> 8,2"))

(struct pt [x y] #:transparent)

(define (format-data LoS)
  (for/list ([str LoS])
    (match-let ([(list one two) (string-split str " -> ")])
      (match-let ([(list x1 y1) (string-split one ",")]
                  [(list x2 y2) (string-split two ",")])
        (list (pt (string->number x1)
                  (string->number y1))
              (pt (string->number x2)
                  (string->number y2)))))))

(define (get-points pts)
  (match-let* ([(list p1 p2) pts]
               [(pt x1 y1) p1]
               [(pt x2 y2) p2])
    (cond
      [(and (= y1 y2) (< x1 x2))
       (for/list ([i (range (add1 (- x2 x1)))]) (pt (+ i x1) (+ 0 y1)))]
      [(and (= y1 y2) (< x2 x1))
       (for/list ([i (range (add1 (- x1 x2)))]) (pt (+ i x2) (+ 0 y1)))]
      [(and (= x1 x2) (< y1 y2))
       (for/list ([i (range (add1 (- y2 y1)))]) (pt (+ 0 x1) (+ i y1)))]
      [(and (= x1 x2) (< y2 y1))
       (for/list ([i (range (add1 (- y1 y2)))]) (pt (+ 0 x1) (+ i y2)))]
;      [else
;       '()])))
      [(and (< x1 x2) (< y1 y2))
       (for/list ([i (range (add1 (- x2 x1)))]) (pt (+ i x1) (+ i y1)))]
      [(and (< x1 x2) (< y2 y1))
       (for/list ([i (range (add1 (- x2 x1)))]) (pt (+ i x1) (- y1 i)))]
      [(and (< x2 x1) (< y1 y2))
       (for/list ([i (range (add1 (- x1 x2)))]) (pt (+ i x2) (- y2 i)))]
      [(and (< x2 x1) (< y2 y1))
       (for/list ([i (range (add1 (- x1 x2)))]) (pt (+ i x2) (+ i y2)))])))

(define (get-duplicate-count LoP)
  (define HASH (make-hash))  
  (for ([p LoP])
    (if (hash-ref HASH p #f)
        (hash-set! HASH p (add1 (hash-ref HASH p)))
        (hash-set! HASH p 1)))
  (~> HASH
      hash->list
      (filter (λ (x) (< 1 (cdr x))) _)
      length))

(~> test
    format-data
    (map get-points _)
    flatten
    get-duplicate-count)

(~> data
    format-data
    (map get-points _)
    flatten
    get-duplicate-count)