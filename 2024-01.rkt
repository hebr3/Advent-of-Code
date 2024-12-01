#lang racket
(require threading)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2024-01.txt"))

(define test "3   4
4   3
2   5
1   3
3   9
3   3")

(define (part-A L)
  (define L* (~> L
                 (string-split _ "\n")
                 (map (λ (x) (string-split x "   ")) _)
                 (map (λ (x) (map string->number x)) _)))
  (apply + (map (λ (a b) (abs (- a b)))
       (sort (map first L*) <)
       (sort (map second L*) <))))

(part-A test)
(part-A data)

;;

(define (part-B L)
  (define L* (~> L
                 (string-split _ "\n")
                 (map (λ (x) (string-split x "   ")) _)
                 (map (λ (x) (map string->number x)) _)))
  (define LEFT (map first L*))
  (define RIGHT (map second L*))
  (for/sum ([i LEFT])
    (* i (count (λ (x) (= x i)) RIGHT))))

(part-B test)