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

(define (check-2? num)
  (let* ([str (number->string num)]
         [len (string-length str)])
    (or (= 1 (set-count (list->set (string->list str))))
        (and (member len (list 4 6 8 10 12 14 16 18 20))
             (= 1 (set-count (list->set (split-twos str)))))
        (and (member len (list 6 9 12))
             (= 1 (set-count (list->set (split-threes str)))))
        (and (member len (list 8 12 16))
             (= 1 (set-count (list->set (split-fours str)))))
        (and (member len (list 10 15 20))
             (= 1 (set-count (list->set (split-fives str))))))))

;; Main Function
(define (part-A input)
  (define ranges (parse-input input))
  (let ([invalid 0])
    (for ([r ranges])
      (for ([i (in-range (first r) (add1 (second r)))])
        (when (check? i)
          (set! invalid (+ i invalid)))))
    invalid))

(part-A test)
(part-A data)

;;

(define (part-B input)
  (define ranges (parse-input input))
  (let ([invalid 0])
    (for ([r ranges])
      (for ([i (in-range (first r) (add1 (second r)))])
        (when (and (< 9 i) (check-2? i))
          (set! invalid (+ i invalid)))))
    invalid))

(part-B test)
(part-B data)
