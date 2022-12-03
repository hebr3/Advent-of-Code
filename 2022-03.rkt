#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2022-03.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")
      ))

(define test '("vJrwpWtwJgWrhcsFMMfFFhFp"
               "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
               "PmmdzqPrVvPwwTWBwg"
               "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
               "ttgJtRGJQctTZtZT"
               "CrZsJsPPZsGzwwsLwLmpwMDw"))

(define (find-dup str)
  (let ([f (substring str 0 (/ (string-length str) 2))]
        [b (substring str (/ (string-length str) 2))])
    (for/or ([c f])
      (and (string-contains? b (string c))
           c))))

(define ht (make-hash))
(for ([c "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"]
      [n (range 1 53)])
  (hash-set! ht c n))

(define (score-dup c)
  (hash-ref ht c))

(define (part-A L)
  (~>> L
      (map find-dup)
      (map score-dup)
      (apply +)))

(part-A test)
(part-A data)

;;---
(define (group-list L)
  (define (iter acc L*)
    (if (empty? L*)
        acc
        (iter (cons (take L* 3) acc) (drop L* 3))))
  (iter '() L))

(define (find-common L)
  (match-let ([(list x y z) L])
    (for/or ([ch x])
      (and (string-contains? y (string ch))
           (string-contains? z (string ch))
           ch))))

(define (part-B L)
  (~>> L
       group-list
       (map find-common)
       (map score-dup)
       (apply +)))

(part-B test)
(part-B data)
