#lang racket
(require threading)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2024-03.txt"))

(define test "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(define mul-pattern
  (pregexp "mul\\((\\d{1,3}),(\\d{1,3})\\)"))

(define (parse-mul str)
  (match-let ([(list a b) (string-split str #rx"mul\\(|,|\\)")])
    (* (string->number a) (string->number b))))

(define (part-A L)
  (let* ([OPS (regexp-match* mul-pattern L)]
         [VALS (map parse-mul OPS)]
         [SUM (for/sum ([i VALS]) i)])
    SUM))

(part-A test)
(part-A data)

;;

(define test2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(define mul-pattern2
  (pregexp "mul\\((\\d{1,3}),(\\d{1,3})\\)|don't\\(\\)|do\\(\\)"))

(define (drop-between L)
  (define (iter L* acc check)
    (cond
      [(empty? L*) acc]
      [(string=? "don't()" (first L*))
       (iter (rest L*) acc #f)]
      [(string=? "do()" (first L*))
       (iter (rest L*) acc #t)]
      [check
       (iter (rest L*) (cons (first L*) acc) check)]
      [else
       (iter (rest L*) acc check)]))
  (iter L '() #t))

(define (part-B L)
  (let* ([OPS (regexp-match* mul-pattern2 L)]
         [ENABLED (drop-between OPS)]
         [VALS (map parse-mul ENABLED)]
         [SUM (for/sum ([i VALS]) i)])
    SUM))

(part-B test2)
(part-B data)