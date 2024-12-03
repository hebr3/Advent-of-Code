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
  (apply * (map string->number (string-split str #rx"mul\\(|,|\\)"))))

(define (part-A L)
  (let* ([LINES (string-split L "\n")])
    (apply +
           (map parse-mul
                (flatten (for/list ([line LINES])
                           (regexp-match* mul-pattern line)))))))

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
  (let* ([LINES (string-split L "\n")])
    (apply +
           (map parse-mul
                (drop-between
                 (flatten (for/list ([line LINES])
                            (regexp-match* mul-pattern2 line))))))))

(part-B test2)
(part-B data)