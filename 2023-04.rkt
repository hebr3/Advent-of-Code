#lang racket
(require threading)
(require racket/match)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2023-04.txt"))

(define test "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

;;

(define (string->set str)
  (~>> str
       string-split
       (map string->number)
       list->set))

(define (intersect-count LoS)
  (match-let ([(list set1 set2) LoS])
    (set-count (set-intersect set1 set2))))

(define (points num)
  (if (zero? num)
      0
      (expt 2 (sub1 num))))

(define (part-A L)
  (define cards (string-split L "\n"))
  (for/sum ([card cards])
    (~> card
        (string-split #rx":|\\|")
        rest
        (map string->set _)
        intersect-count
        points)))
  

(part-A test)
(part-A data)

;;

(define (get-card-number str)
  (string->number (second (string-split str))))

(define (part-B L)
  (define cards (string-split L "\n"))
  (define WIN-COUNT-HASH (make-hash))
  (define CARD-COUNT-HASH (make-hash))
  (define (score-card card)
    (match-let ([(list card-num your-nums winning-nums) (string-split card #rx":|\\|")])
      (define num (get-card-number card-num))
      (~> (list your-nums winning-nums)
          (map string->set _)
          intersect-count
          (hash-set! WIN-COUNT-HASH num _)
          )
      (hash-set! CARD-COUNT-HASH num 1)))
  
  (for ([card cards])
    (~> card
        score-card))

  (define (hash-inc idx num cnt)
    (for ([i num])
      (define old-cnt (hash-ref CARD-COUNT-HASH (add1 (+ idx i))))
      (hash-set! CARD-COUNT-HASH (add1 (+ idx i)) (+ old-cnt cnt))))
    
  (for ([card cards])
    (define num (string->number (second (string-split card #px"\\s+|:"))))
    (define val (hash-ref WIN-COUNT-HASH num))
    (hash-inc num val (hash-ref CARD-COUNT-HASH num)))
    
  (~>> CARD-COUNT-HASH
       hash->list
       (map cdr)
       (apply +)))

(part-B test)
(part-B data)
