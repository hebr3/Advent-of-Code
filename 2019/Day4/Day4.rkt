#lang racket
(require rackunit)
(require racket/match)

(define (double? number)
  (let* ([str (number->string number)]
         [list-of-char (string->list str)])
    (for/or ([a list-of-char][b (cdr list-of-char)])
      (char=? a b))))

(define (inc? number)
  (let* ([str (number->string number)]
         [list-of-char (string->list str)])         
    (for/and ([a list-of-char][b (cdr list-of-char)])
      (<= (string->number (string a))
          (string->number (string b))))))

(define (odd-group? number)
  (match-let ([(num a b c d e f)
               (number->num number)])
    (or (and (= a b c)
             (not (= a b c d)))
        (and (= b c d)
             (not (= b c d e)))
        (and (or (= c d e)
                 (= d e f))
             (not (= c d e f)))
        (and (or (= a b c d e)
                 (= b c d e f))
             (= a b c d e f)))))
        
(struct num [a b c d e f] #:transparent)
(define (number->num n)
  (let ([a (quotient n 100000)]
        [b (modulo (quotient n 10000) 10)]
        [c (modulo (quotient n 1000) 10)]
        [d (modulo (quotient n 100) 10)]
        [e (modulo (quotient n 10) 10)]
        [f (modulo n 10)])
    (num a b c d e f)))
            
;(check-equal? (map double? '(111111 223450 123789))
;              '(#t #t #f)
;              "test double?")
;(check-equal? (map inc? '(111111 223450 123789))
;              '(#t #f #t)
;              "test inc?")

(define (Day4a m n)
  (for/sum ([i (range m n)])
    (if (and (double? i) (inc? i))
        1
        0)))

(odd-group? 111122)
;(check-equal? (map odd-group? '(112233 123444 111122))
;              '(#f #t #f)
;              "test odd-group?")

;(check-equal? (Day4a 264793 803935) 966 "Check both from 264793 to 803935")