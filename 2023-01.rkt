#lang racket
(require threading)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2023-01.txt"))

(define test "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(define (part-A L)
  (define (first-and-last s)
    (list (first s) (last s)))
  (~> L
      (string-split "\n")
      (map (λ (s) (string->list s)) _)
      (map (λ (l) (filter (λ (c) (member c (list #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))) l)) _) 
      (map (λ (l) (first-and-last l)) _)
      (map (λ (l) (list->string l)) _)
      (map (λ (s) (string->number s)) _)
      (apply + _)
      ))

(part-A test)
(part-A data)

;;

(define valid-Hash (make-hash))
(hash-set! valid-Hash "one" 1)
(hash-set! valid-Hash "two" 2)
(hash-set! valid-Hash "three" 3)
(hash-set! valid-Hash "four" 4)
(hash-set! valid-Hash "five" 5)
(hash-set! valid-Hash "six" 6)
(hash-set! valid-Hash "seven" 7)
(hash-set! valid-Hash "eight" 8)
(hash-set! valid-Hash "nine" 9)
(hash-set! valid-Hash "1" 1)
(hash-set! valid-Hash "2" 2)
(hash-set! valid-Hash "3" 3)
(hash-set! valid-Hash "4" 4)
(hash-set! valid-Hash "5" 5)
(hash-set! valid-Hash "6" 6)
(hash-set! valid-Hash "7" 7)
(hash-set! valid-Hash "8" 8)
(hash-set! valid-Hash "9" 9)
(hash-set! valid-Hash "0" 0)

(define test2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(define (get-first-num str)
  (define (iter i f)
    ;(println (substring str i f))
    (cond
      [(hash-has-key? valid-Hash (substring str i f))
       (hash-ref valid-Hash (substring str i f))]
      [(< (sub1 (string-length str)) f) (iter (+ 1 i) (+ 2 i))]
      [(< 5 (- f i)) (iter (+ 1 i) (+ 2 i))]
      [else (iter i (+ 1 f))]))
  (iter 0 1))
    
(define (get-last-num str)
  (define (iter i f)
    (define sub (substring str (- (string-length str) f) (- (string-length str) i)))
    (cond
      [(hash-has-key? valid-Hash sub)
       (hash-ref valid-Hash sub)]
      [(< (sub1 (string-length str)) f) (iter (+ 1 i) (+ 2 i))] 
      [(< 5 (- f i)) (iter (+ 1 i) (+ 2 i))]
      [else (iter i (+ 1 f))]))
  (iter 0 1))

(define (first-and-last2 str)
  (let ([f (get-first-num str)]
        [l (get-last-num str)])
    (+ (* 10 f) l)))

(define (part-B L)
  (~> L
      (string-split "\n")
      (map (λ (s) (first-and-last2 s)) _)
      (apply + _)))

(part-B test2)
(part-B data)
