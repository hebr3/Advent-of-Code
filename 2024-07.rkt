#lang racket
(require threading)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2024-07.txt"))

(define test "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
")

(define (parse-line str)
  (match-let ([(list val parts) (string-split str ": ")])
    (list (string->number val)
          (map string->number (string-split parts " ")))))

(define (test-equation eq)
  (match-let ([(list val parts) eq])
    (let ([RESULTS (mutable-set)])
      (define (iter L acc)
        (cond
          [(empty? L) (set-add! RESULTS acc)]
          [else
           (iter (rest L) (+ acc (first L)))
           (iter (rest L) (* acc (first L)))]))
      (iter (rest parts) (first parts))
      (if (set-member? RESULTS val)
           val
           0))))

(define (part-A L)
  (let* ([LINES (string-split L "\n")]
         [EQUATIONS (map parse-line LINES)])
    (for/sum ([eq EQUATIONS])
      (test-equation eq))))

(part-A test)
(part-A data)

;;

(define (barbar a b)
  (string->number
   (string-append (number->string a)
                  (number->string b))))
  
(define (test-equation-2 eq)
  (match-let ([(list val parts) eq])
    (let ([RESULTS (mutable-set)])
      (define (iter L acc)
        (cond
          [(empty? L) (set-add! RESULTS acc)]
          [else
           (iter (rest L) (+ acc (first L)))
           (iter (rest L) (* acc (first L)))
           (iter (rest L) (barbar acc (first L)))]))
      (iter (rest parts) (first parts))
      (if (set-member? RESULTS val)
           val
           0))))

(define (part-B L)
  (let* ([LINES (string-split L "\n")]
         [EQUATIONS (map parse-line LINES)])
    (for/sum ([eq EQUATIONS])
      (test-equation-2 eq))))

(part-B test)
(part-B data)
