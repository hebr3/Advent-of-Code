#lang racket
(require threading)
(require racket/match)

;; Prep
(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)
      (string-split "\n")))

(define (test->list-of-strings str)
  (string-split str "\n"))

;; Structs

;; Functions
(define (list-ref-xy L x y)
  (list-ref (list-ref L y) x))  

(define (empty-seat? x y list-of-list-of-seats)
  (let ([Y (length list-of-list-of-seats)]
        [X (length (first list-of-list-of-seats))])
    (or (< x 0) (< y 0) (= x X) (= y Y)
        (char=? #\L (list-ref-xy list-of-list-of-seats x y))
        (char=? #\. (list-ref-xy list-of-list-of-seats x y)))))

(define (count-neighbors x y list-of-list-of-seats)
  (+ (if (empty-seat? (sub1 x) (sub1 y) list-of-list-of-seats) 0 1)
     (if (empty-seat? x (sub1 y) list-of-list-of-seats) 0 1)
     (if (empty-seat? (add1 x) (sub1 y) list-of-list-of-seats) 0 1)
       
     (if (empty-seat? (sub1 x) y list-of-list-of-seats) 0 1)
     (if (empty-seat? (add1 x) y list-of-list-of-seats) 0 1)
       
     (if (empty-seat? (sub1 x) (add1 y) list-of-list-of-seats) 0 1)
     (if (empty-seat? x (add1 y) list-of-list-of-seats) 0 1)
     (if (empty-seat? (add1 x) (add1 y) list-of-list-of-seats) 0 1)))

(define (update-seat x y list-of-list-of-seats)
  (let ([S (list-ref-xy list-of-list-of-seats x y)]
        [Y (length list-of-list-of-seats)]
        [X (length (first list-of-list-of-seats))])        
    (cond
      [(char=? #\. S) #\.]
      [(and (char=? #\L S)
            (zero? (count-neighbors x y list-of-list-of-seats))) #\#]
      [(char=? #\L S) #\L]
      [(and (char=? #\# S)
            (< 3 (count-neighbors x y list-of-list-of-seats))) #\L]
      [else #\#])))



(define (update-list list-of-list-of-seats cnt)
  (define (iter lst)
    (for/list ([y (length lst)])
      (for/list ([x (length (first lst))])
        (update-seat x y lst))))
  (let* ([next (iter list-of-list-of-seats)]
         [cnt* (count (λ (x) (char=? #\# x)) (flatten list-of-list-of-seats))])
    (if (= cnt cnt*)
        (displayln cnt*)
        (update-list next cnt*))))

;; Data
(define test
  (~>> "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"
       test->list-of-strings
       (map string->list)))

(define data
  (~>> "input.txt"
       file->list-of-strings
       (map string->list)))

;; Puzzle
(display "test 1: ")
(~> test
    (update-list -1))

(display "one: ")
(~> data     
    (update-list -1))

;(display "test 2: ")
;(~>> test)
;
;(display "two: ")
;(~>> data)

