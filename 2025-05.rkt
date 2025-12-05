#lang racket

(require rackunit)
(require racket/set)

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

;; Test data
(define data (input->data "input/2025-05.txt"))

(define test "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

;; Helper Function
(define (string->lon str)
  (map string->number (string-split str "-")))

(define (string->function str)
  (λ (x) (begin
           (for/or ([line (string-split str "\n")])
             (match-define (list start end) (string->lon line))
;             (when (<= start x end)
;               (println (list start x end)))
             (<= start x end)))))

(define (string->numbers str)
  (for/list ([line (string-split str "\n")])
    (string->number line)))

(define (string->data str)
  (match-define (list top bot) (string-split str "\n\n"))
  (list top bot))

(define (overlap? a b)
  (match-define (list min-a max-a) a)
  (match-define (list min-b max-b) b)
  (not (or (< max-a min-b)
           (< max-b min-a))))

(define (join a b)
  (match-define (list min-a max-a) a)
  (match-define (list min-b max-b) b)
  (list (min min-a min-b) (max max-a max-b)))

(check-equal? (join (list 1 5) (list 3 6)) (list 1 6))

(define (shrink-ranges lor)
  (define (iter acc)
    (define len (length acc))
    (define S (mutable-set))
    (for ([r acc]) (set-add! S r))
    (println (set->list S))
    (for ([a acc][b (rest acc)])
      (when (overlap? a b)
        (set-remove! S a)
        (set-remove! S b)
        (set-add! S (join a b))))
;    (println (set->list S))
    (if (= len (set-count S))
        (set->list S)
        (iter (sort (set->list S) < #:key car))))
  (iter lor))

;; Main Function
(define (part-A input)
  (define data (string->data input))
  (define fresh? (string->function (first data)))
  (define ingredients (string->numbers (second data)))
  (define still-fresh (filter fresh? ingredients))
  (length still-fresh))

(check-equal? (part-A test) 3)

(part-A data)

;;

(define (part-B input)
  (define data (string->data input))
  (define rngs (sort (map string->lon (string-split (first data) "\n")) < #:key car))
  (define min-rngs (shrink-ranges rngs))
  (for/sum ([r min-rngs])
    (- (second r) (first r) -1)))
  
(check-equal? (part-B test) 14)

(part-B data)
