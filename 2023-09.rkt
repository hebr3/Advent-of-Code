#lang racket
(require threading)
(require racket/match)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2023-09.txt"))

(define test "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

;;

(define (zero-list? l)
  (for/and ([i l]) (zero? i)))

(define (next-list l)
  (for/list ([i l][j (rest l)])
    (- j i)))

(define (next-val l)
  (for/sum ([i l])
    (last i)))

(define (part-A L)
  (let* ([rows (string-split L "\n")]
         [los (map string-split rows)]
         [histories (map (λ (l) (map string->number l)) los)])
    (define (iter l acc)
      (if (zero-list? l)
          (cons l acc)
          (iter (next-list l) (cons l acc))))
    (for/sum ([h histories])
      (next-val (iter h '())))))
  

(part-A test)
(part-A data)

;;

(define (prev-val l)
  (define (iter lst acc)
    (if (empty? lst)
        acc
        (iter (rest lst) (- (first lst) acc))))
  (let ([fst (for/list ([i l]) (first i))])
    (iter (rest fst) (first fst))))
      

(define (part-B L)
  (let* ([rows (string-split L "\n")]
         [los (map string-split rows)]
         [histories (map (λ (l) (map string->number l)) los)])
    (define (iter l acc)
      (if (zero-list? l)
          (cons l acc)
          (iter (next-list l) (cons l acc))))
    (for/sum ([h histories])
       (prev-val (iter h '())))))
          
(part-B test)
(part-B data)
