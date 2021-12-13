#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2021-06.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")))

(define test
  '("3,4,3,1,2"))

(define identity (λ (x) x))

(define (bundle-list L)
  (vector (car L) (length L)))

(define (format-data str)
  (~> str
      car
      (string-split _ ",")
      (map string->number _)
      (group-by identity _)
      (map bundle-list _)))

(define (flatten-fish-set LoF)
  (define HASH (make-hash))
  (for ([f (flatten LoF)])
    (match-let ([(vector age cnt) f])
      (let ([fish-cnt (hash-ref HASH age #f)])
      (if fish-cnt
          (hash-set! HASH age (+ cnt fish-cnt))
          (hash-set! HASH age cnt)))))
  (~> HASH
      hash->list
      (map (λ (x) (vector (car x) (cdr x))) _)))

(define (age-fish-set fish-set)
  (match-let ([(vector age cnt) fish-set])
    (if (zero? age)
        (list (vector 6 cnt) (vector 8 cnt))
        (list (vector (sub1 age) cnt)))))

(define (age-all-fish-N-days LoF N)
  (if (zero? N)
      LoF
      (~> LoF
          (map age-fish-set _)
          flatten-fish-set
          (age-all-fish-N-days _ (sub1 N)))))

(define (sum L)
  (apply + L))
         
(~> test
    format-data
    (age-all-fish-N-days _ 80)
    (map (λ (x) (vector-ref x 1)) _)
    sum)
    

(~> data
    format-data
    (age-all-fish-N-days _ 80)
    ;length
    (map (λ (x) (vector-ref x 1)) _)
    sum)

(~> test
    format-data
    (age-all-fish-N-days _ 256)
    ;length
    (map (λ (x) (vector-ref x 1)) _)
    sum)

(~> data
    format-data
    (age-all-fish-N-days _ 256)
    ;length
    (map (λ (x) (vector-ref x 1)) _)
    sum)