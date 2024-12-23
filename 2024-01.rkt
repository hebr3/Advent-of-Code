#lang racket

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

(define data (input->data "input/2024-01.txt"))

(define test "3   4
4   3
2   5
1   3
3   9
3   3")

(define (str->pair str)
  (map string->number (string-split str "   ")))

(define (part-A L)
  (let* ([LINES (string-split L "\n")]
         [PARTS (map str->pair LINES)]
         [LEFT  (sort (map first PARTS) <)]
         [RIGHT (sort (map second PARTS) <)]
         [DIFF  (map (λ (a b) (abs (- a b))) LEFT RIGHT)]
         [SUM   (apply + DIFF)])
    SUM))
         
(part-A test)
(part-A data)

;;

(define (part-B L)
  (let* ([LINES (string-split L "\n")]
         [PARTS (map str->pair LINES)]
         [LEFT  (sort (map first PARTS) <)]
         [RIGHT (sort (map second PARTS) <)]
         [SCORE (for/sum ([i LEFT])
                  (* i (count (λ (x) (= x i)) RIGHT)))])
    SCORE))

(part-B test)
(part-B data)