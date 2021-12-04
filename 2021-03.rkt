#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2021-03.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")))

(define test
  '("00100"
    "11110"
    "10110"
    "10111"
    "10101"
    "01111"
    "00111"
    "11100"
    "10000"
    "11001"
    "00010"
    "01010"))


(*
 (string->number
  (string-join (list "#b"
                     (apply string
                            (map (λ (x) (if (> x (/ (length test) 2)) #\1 #\0))
                                 (for/list ([i 5])
                                   (count (λ (c) (char=? #\1 c))
                                          (for/list ([l test])
                                            (string-ref l i)))))))
               ""))
 (string->number
  (string-join (list "#b"
                     (apply string
                            (map (λ (x) (if (< x (/ (length test) 2)) #\1 #\0))
                                 (for/list ([i 5])
                                   (count (λ (c) (char=? #\1 c))
                                          (for/list ([l test])
                                            (string-ref l i)))))))
               "")))

(*
 (string->number
  (string-join (list "#b"
                     (apply string
                            (map (λ (x) (if (> x (/ (length data) 2)) #\1 #\0))
                                 (for/list ([i 12])
                                   (count (λ (c) (char=? #\1 c))
                                          (for/list ([l data])
                                            (string-ref l i)))))))
               ""))
 (string->number
  (string-join (list "#b"
                     (apply string
                            (map (λ (x) (if (< x (/ (length data) 2)) #\1 #\0))
                                 (for/list ([i 12])
                                   (count (λ (c) (char=? #\1 c))
                                          (for/list ([l data])
                                            (string-ref l i)))))))
               "")))

(define (find-most-common L)
  (if (= 1 (length L))
      L
      (~> L
          cdr
          find-most-common)))

(find-most-common test)

(define (return-most-common-at-i L i)
  (list-ref 
   (sort
    (map (λ (str) (string-ref str i)) L)
    char>?)
   (floor (/ (length L) 2))))

(define (filter-char-for-most-common-at-i L i)
  (filter (λ (str) (char=? (return-most-common-at-i L i) (string-ref str i))) L))

(return-most-common-at-i test 1)

(let ([L test])
  (for ([i 5])
    (println L)
    (set! L (filter-char-for-most-common-at-i L i))
    (println L))
  L)

(filter-char-for-most-common-at-i test 0)