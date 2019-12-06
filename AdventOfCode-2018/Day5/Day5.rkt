#lang racket

(define test1
  (list "abcdef"
        "bababc"
        "abbcde"
        "abcccd"
        "aabcdd"
        "abcdee"
        "ababab"))

(define (count-num num str)
  (count (λ (x) x)
         (map (λ (x) (= num (length x)))
              (group-by (λ (x) x) (string->list str)))))

(define (count2 x)
  (count-num 2 x))
(define (count3 x)
  (count-num 3 x))

(* (count (λ (x) (< 0 x))
          (map count2 test1))
   (count (λ (x) (< 0 x))
          (map count3 test1)))

(define input
  (file->lines "input.txt"))

(* (count (λ (x) (< 0 x))
          (map count2 input))
   (count (λ (x) (< 0 x))
          (map count3 input)))

(define test2
  (list "abcde"
        "fghij"
        "klmno"
        "pqrst"
        "fguij"
        "axcye"
        "wvxyz"))

(let ([F (car test2)][S (cadr test2)])
  (for/list ([f F][s S]) (char=? f s)))