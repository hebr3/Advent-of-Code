#lang racket

(require rackunit)
(require racket/set)

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

;; Test data
(define data (input->data "input/2025-06.txt"))

(define test "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")

;; Helper Function
(define (string->vals str)
  (map (λ (x) (or (string->number x) x))
       (filter non-empty-string?
               (string-split str " "))))

(define (vals->cols vals)
  (for/list ([i (length (first vals))])
    (for/list ([j (length vals)])
      (list-ref (list-ref vals j) i))))

(define (calc-col col)
  (if (member "*" col)
      (apply * (filter number? col))
      (apply + (filter number? col))))

(define (count-rows input)
  (+ 1 (for/sum ([ch input])
         (if (char=? #\newline ch) 1 0))))

(define (lines->cols lines num-rows)
  (for/list ([i (string-length (first lines))])
    (for/list ([j (length lines)])
      (string-ref (list-ref lines j) i))))

(define (spaces? loc)
  (empty? (remove* (list #\space) loc)))

(check-equal? (spaces? (list #\space #\space #\space #\space)) #t)
(check-equal? (spaces? (list #\1 #\space #\space #\*)) #f)

(struct stack [op vals] #:transparent)

(define (loc->number loc)
  (string->number (apply string (remove* (list #\space #\* #\+) loc))))

(check-equal? (loc->number (list #\1 #\space #\space #\*)) 1)
(check-equal? (loc->number (list #\2 #\4 #\space #\space)) 24)

(define (loloc->number loloc)
  (if (member #\* (flatten loloc))
      (for/product ([loc loloc]) (loc->number loc))
      (for/sum ([loc loloc]) (loc->number loc))))

(check-equal? (loloc->number (list (list #\1 #\space #\*) (list #\2 #\0 #\space))) 20)
(check-equal? (loloc->number (list (list #\1 #\space #\+) (list #\2 #\2 #\space))) 23)
(check-equal? (loloc->number '((#\1 #\space #\space #\*) (#\2 #\4 #\space #\space) (#\3 #\5 #\6 #\space))) 8544)
 
(define (group-cols cols)
  (define (iter cols* acc)
    (define head (takef cols* (λ (x) (not (spaces? x)))))
    ;(println head)
    (define val (loloc->number head))
    (cond
      [(> (length cols*) (length head))
       (define tail (drop cols* (add1 (length head))))
       ;(println tail)
       (iter tail (cons val acc))]
      [else
       (cons val acc)]))
  (iter cols '()))

;; Main Function
(define (part-A input)
  (define lines (string-split input "\n"))
  (define vals (map string->vals lines))
  (define cols (vals->cols vals))
  (for/sum ([col cols])
    (calc-col col)))

(check-equal? (part-A test) 4277556)

(part-A data)

;;

(define (part-B input)
  (define num-rows (count-rows input))
  (define lines (string-split input "\n"))
  (define cols (lines->cols lines num-rows))
  (apply + (group-cols cols)))
  
(check-equal? (part-B test) 3263827)

(part-B data)
