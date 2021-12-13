#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2021-06.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")))

(define test
  '("3,4,3,1,2"))

(define (initialize LoN)
  (for/list ([i 9]) (cons i (count (λ (n) (= n i)) LoN))))

(define (format-data str)
  (~> str
      car
      (string-split _ ",")
      (map string->number _)
      initialize))

(define (update-list L)
  (for/list ([i 9])
    (cond
      [(= i 6)
       (cons i (+ (cdr (list-ref L 0))
                  (cdr (list-ref L 7))))]
      [(= i 8)
       (cons i (cdr (list-ref L 0)))]
      [else
       (cons i (cdr (list-ref L (add1 i))))])))
    
(define (update-N-times L N)
  (if (zero? N)
      L
      (update-N-times (update-list L) (sub1 N))))

(for/sum ([i (~> test
                 format-data
                 (update-N-times _ 80))])
  (cdr i))
    
(for/sum ([i (~> data
                 format-data
                 (update-N-times _ 80))])
  (cdr i))

(for/sum ([i (~> test
                 format-data
                 (update-N-times _ 256))])
  (cdr i))

(for/sum ([i (~> data
                 format-data
                 (update-N-times _ 256))])
  (cdr i))