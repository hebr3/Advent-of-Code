#lang racket

(require data/heap)

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

;; Test data
(define data (input->data "input/2025-02.txt"))

(define test "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

;; Helper Function
(define (parse-range str)
  (map string->number (string-split str "-")))

(define (parse-input input)
  (map parse-range (string-split input ",")))

(define (check? num)
  (regexp-match? #px"^(\\d{1,})\\1$" (number->string num)))

(define (check-2? num)
  (regexp-match? #px"^(\\d+?)\\1+$" (number->string num)))

(define (valid-length? num)
  (define len (string-length (number->string num)))
  (or (even? len)
      (zero? (modulo len 3))
      (zero? (modulo len 5))))

(define (parallel-range-sum ranges check-fn)
  (define threads
    (for/list ([r ranges])
      (thread
       #:pool 'own
       #:keep 'results
       (λ ()
         (for/sum ([i (in-range (first r) (add1 (second r)))] #:when (valid-length? i))
           (if (check-fn i) i 0))))))
  (for/sum ([t threads])
    (thread-wait t)))

;; Main Function
(define (part-A input)
  (define ranges (parse-input input))
  (parallel-range-sum ranges check?))

(part-A test)
(part-A data)

;;

(define (part-B input)
  (define ranges (parse-input input))
  (parallel-range-sum ranges check-2?))

(part-B test)
(part-B data)
