#lang racket
(require threading)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2024-11.txt"))

(define test "125 17
")

(define lookup-HT (make-hash))

(define (zero->one x)
  1)
(define (even-digits? x)
  (even? (string-length (number->string x))))
(define (break-number x)
  (let* ([STR (number->string x)]
         [LEN (string-length STR)])
    (list (string->number (substring STR 0 (/ LEN 2)))
          (string->number (substring STR (/ LEN 2))))))
(define (mult-2024 x)
  (* x 2024))

(define (blink x cnt)
  (cond
    [(hash-has-key? lookup-HT (list x cnt)) (hash-ref lookup-HT (list x cnt))]
    [(zero? x)
     (let ([next (zero->one x)])
       (hash-set! lookup-HT (list x cnt) next)
       next)]
    [(even-digits? x)
     (let ([next (break-number x)])
       (hash-set! lookup-HT (list x cnt) next)
       next)]
    [else 
     (let ([next (mult-2024 x)])
       (hash-set! lookup-HT (list x cnt) next)
       next)]))

(define (part-A L)
  (define INIT-STONES (map string->number (string-split L)))
  (define (iter L cnt)
    ;(println L)
    (cond
      [(zero? cnt) L]
      [else (iter (flatten (map (λ (x) (blink x (- 25 cnt))) L)) (sub1 cnt))]))
  (length (iter INIT-STONES 25)))


(part-A test)
(part-A data)

;;

(define (part-B L)
  (define INIT-STONES (map string->number (string-split L)))
  (define (iter L cnt)
    (println (list (- 75 cnt) (length L)))
    (cond
      [(zero? cnt) L]
      [else (iter (flatten (map (λ (x) (blink x (- 75 cnt))) L)) (sub1 cnt))]))
  (length (iter INIT-STONES 75)))

;(part-B test)
;(part-B data)
