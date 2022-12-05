#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2016-03.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")
      ))

(define test '("5 10 25"))

;; helper
(define (string-of-numbers->list-of-numbers son)
  (~>> son
       (string-split _ #px"\\W")
       (filter non-empty-string?)
       (map string->number)))

;; functions
(define (triangle? LoN)
  (let ([S (sort LoN <)])
    (match-let ([(list a b c) S])
      (< c (+ a b)))))

(define (part-A L)
  (~>> L
       (map string-of-numbers->list-of-numbers)
       (filter triangle?)
       length))

(part-A test)
(part-A data)

;; Part B
(define test2 '("101 301 501"
                "102 302 502"
                "103 303 503"
                "201 401 601"
                "202 402 602"
                "203 403 603"))

(define (group-vertical LoLoN)
  (define (group-sets L)
    (match-let ([(list A B C) L])
      (match-let ([(list a1 a2 a3) A]
                  [(list b1 b2 b3) B]
                  [(list c1 c2 c3) C])
        (list (list a1 b1 c1)
              (list a2 b2 c2)
              (list a3 b3 c3)))))
  (define (iter acc L*)
    (if (empty? L*)
        acc
        (iter (cons (group-sets (take L* 3)) acc) (drop L* 3))))
  (iter '() LoLoN))


(define (part-B L)
  (~>> L
       (map string-of-numbers->list-of-numbers)
       group-vertical
       (map (λ (x) (filter triangle? x)))
       (map length)
       (apply +)))

(part-B test2)
(part-B data)
