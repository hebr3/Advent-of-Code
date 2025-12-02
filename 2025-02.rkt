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
  (let* ([str (number->string num)]
         [len (string-length str)])
    (string=? (substring str 0 (floor (/ len 2)))
              (substring str (floor (/ len 2))))))

(define (split-twos str)
  (for/list ([i (in-range (floor (/ (string-length str) 2)))])
    (list->string (list (string-ref str (+ 0 (* 2 i)))
                        (string-ref str (+ 1 (* 2 i)))))))

(define (split-threes str)
  (for/list ([i (in-range (floor (/ (string-length str) 3)))])
    (list->string (list (string-ref str (+ 0 (* 3 i)))
                        (string-ref str (+ 1 (* 3 i)))
                        (string-ref str (+ 2 (* 3 i)))))))

(define (split-fours str)
  (for/list ([i (in-range (floor (/ (string-length str) 4)))])
    (list->string (list (string-ref str (+ 0 (* 4 i)))
                        (string-ref str (+ 1 (* 4 i)))
                        (string-ref str (+ 2 (* 4 i)))
                        (string-ref str (+ 3 (* 4 i)))))))

(define (split-fives str)
  (for/list ([i (in-range (floor (/ (string-length str) 5)))])
    (list->string (list (string-ref str (+ 0 (* 5 i)))
                        (string-ref str (+ 1 (* 5 i)))
                        (string-ref str (+ 2 (* 5 i)))
                        (string-ref str (+ 3 (* 5 i)))
                        (string-ref str (+ 4 (* 5 i)))))))

(define (chunk str n)
  (for/list ([i (in-range 0 (floor (/ (string-length str) n)))])
    (substring str (* i n) (+ (* i n) n))))

(define (repeated? str n)
  (let ([chunks (chunk str n)])
    (= 1 (set-count (list->set chunks)))))

(define (check-2? num)
  (let* ([str (number->string num)]
         [len (string-length str)])
    (or (and (< 1 len)
             (= 1 (set-count (list->set (string->list str)))))
        (and (member len (list 4 6 8 10 12 14 16 18 20))
             (repeated? str 2))
        (and (member len (list 6 9 12))
             (repeated? str 3))
        (and (member len (list 8 12 16))
             (repeated? str 4))
        (and (member len (list 10 15 20))
             (repeated? str 5)))))

;; Main Function
(define (part-A input)
  (define ranges (parse-input input))
  (for/sum ([r ranges])
    (for/sum ([i (in-range (first r) (add1 (second r)))])
      (if (check? i) i 0))))

(part-A test)
(part-A data)

;;

(define (part-B input)
  (define ranges (parse-input input))
  (for/sum ([r ranges])
    (for/sum ([i (in-range (first r) (add1 (second r)))])
      (if (check-2? i) i 0))))

(part-B test)
(part-B data)
