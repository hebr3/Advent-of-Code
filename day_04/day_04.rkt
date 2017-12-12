#lang racket
(require threading)

(define t1 (list "aa" "bb" "cc" "dd" "ee"))
(define t2 (list "aa" "bb" "cc" "dd" "aa"))
(define t3 (list "aa" "bb" "cc" "dd" "aaa"))

(define t4 (list t1 t2 t3))

(define in
  (~> "input.txt"
      file->lines
      (map string-split _)))

(define (check? lst)
  (not
   (for/or ([i (sub1 (length lst))])
     (let* ([lst2 (drop lst i)]
            [head (car lst2)]
            [tail (cdr lst2)])
       (ormap (λ (x) (string=? head x)) tail)))))

(count (λ (x) x)
       (map check? in))

(define (sort-string str)
  (~> str
      string->list
      (sort _ char<?)
      list->string))

(define in2
  (map (λ (x) (map sort-string x)) in))

(count (λ (x) x)
       (map check? in2))