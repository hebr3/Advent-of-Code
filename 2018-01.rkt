#lang racket
(require threading)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2018-01.txt"))

(define test1 "+1
-2
+3
+1")
(define test2 "+1
+1
+1")
(define test3 "+1
+1
-2")
(define test4 "-1
-2
-3")

;;

(define (part-A L)
  (define changes (string-split L "\n"))
  (~>> changes
       (map string->number)
       (apply +)
       ))

(part-A test1)
(part-A test2)
(part-A test3)
(part-A test4)
(part-A data)

;;

(define test5 "+1
-1")
(define test6 "+3
+3
+4
-2
-4")
(define test7 "-6
+3
+4
-2
-4")
(define test8 "+7
+7
-2
-7
-4")

(define (part-B L)
  (define changes (map string->number (string-split L "\n")))
  (define LOOKUP (make-hash))
  (define count 0)
  (hash-set! LOOKUP 0 #t)
  (define (iter sum i)
    (define sum* (+ sum (list-ref changes i)))
    (set! count (+ 1 count))
    (cond
      [(< 1000000 count) count]
      [(hash-has-key? LOOKUP sum*) (list sum* count)]
      [else
       (hash-set! LOOKUP sum* #t)
       (iter sum* (modulo (+ 1 i) (length changes)))]))
  (iter 0 0))

(part-B test5)
(part-B test6)
(part-B test7)
(part-B test8)
(part-B data)
    
