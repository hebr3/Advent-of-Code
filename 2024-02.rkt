#lang racket
(require threading)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2024-02.txt"))

(define test "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(define (str->pair str)
  (map string->number (string-split str " ")))

(define (increasing? L)
  (for/and ([i L][j (rest L)]) (<= 1 (- j i) 3)))
(define (decreasing? L)
  (for/and ([i L][j (rest L)]) (<= 1 (- i j) 3)))
(define (valid-report? L)
  (or (increasing? L)
      (decreasing? L)))

(define (part-A L)
  (let* ([LINES (string-split L "\n")]
         [LIST-OF-NUMBERS (map str->pair LINES)]
         [VALID-LIST (map valid-report? LIST-OF-NUMBERS)])
    (count (λ (x) x) VALID-LIST)))
         
(part-A test)
(part-A data)

;;

(define (drop-index L idx)
  (cond
    [(zero? idx) (rest L)]
    [(= idx (sub1 (length L))) (take L idx)]
    [else
     (flatten (list (take L idx) (drop L (add1 idx))))]))

(define (increasing-v2? L)
  (or (increasing? L)
      (for/or ([i (length L)]) (increasing? (drop-index L i)))))

(define (decreasing-v2? L)
  (or (decreasing? L)
      (for/or ([i (length L)]) (decreasing? (drop-index L i)))))

(define (valid-report-v2? L)
  (or (increasing-v2? L)
      (decreasing-v2? L)))

(define (part-B L)
  (let* ([LINES (string-split L "\n")]
         [LIST-OF-NUMBERS (map str->pair LINES)]
         [VALID-LIST (map valid-report-v2? LIST-OF-NUMBERS)])
    (count (λ (x) x) VALID-LIST)))

(part-B test)
(part-B data)