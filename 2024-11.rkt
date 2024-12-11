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

(define (blink x)
  (cond
    [(hash-has-key? lookup-HT x) (hash-ref lookup-HT x)]
    [(zero? x)
     (let ([next (zero->one x)])
       (hash-set! lookup-HT x next)
       next)]
    [(even-digits? x)
     (let ([next (break-number x)])
       (hash-set! lookup-HT x next)
       next)]
    [else 
     (let ([next (mult-2024 x)])
       (hash-set! lookup-HT x next)
       next)]))

(define (part-A L)
  (define INIT-STONES (map string->number (string-split L)))
  (define (iter L cnt)
    ;(println L)
    (cond
      [(zero? cnt) L]
      [else (iter (flatten (map (λ (x) (blink x)) L)) (sub1 cnt))]))
  (length (iter INIT-STONES 25)))


(part-A test) ;55312
(part-A data) ;197357

;;

(struct stone [val cnt] #:transparent)

(define (blink-2 val)
  ;(println val)
  (match-let ([(stone x cnt) val]) 
    (cond
      [(zero? x) (zero->one x)]
      [(even-digits? x) (break-number x)]
      [else (mult-2024 x)])))

(define (combine-stones L HT)
  L)

(define (part-B L)
  (define initial-HT (for/hash ([i (map string->number (string-split L))]) (values i 1)))
  (define N 75)
  (define (iter last-HT cnt)
    (define next-HT (make-hash))
    (define (update-HT s c)
      (hash-set! next-HT s (+ c (hash-ref next-HT s 0))))
    (cond
      [(zero? cnt) last-HT]
      [else 
       ;(println cnt)
       (for ([stone-count (hash->list last-HT)])
         (match-let ([(cons stne cnt) stone-count])
           (cond
             [(zero? stne)
              (update-HT 1 cnt)]
             [(even-digits? stne)
              (match-let ([(list a b) (break-number stne)])
                (update-HT a cnt)
                (update-HT b cnt))]
             [else
              (update-HT (* stne 2024) cnt)])))
       (iter next-HT (sub1 cnt))]))
  (for/sum ([i (hash->list (iter initial-HT N))]) (cdr i)))

(part-B test)
(part-B data)
