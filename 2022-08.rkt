#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2022-08.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")
      ))

(define test '("30373"
               "25512"
               "65332"
               "33549"
               "35390"))

;;---

(define (visible-count grid)
  (define (get-xy x y)
    (~> grid
        (list-ref y)
        (string-ref x)
        string
        string->number))
  (define (get-row y)
    (for/list ([i (string-length (first grid))])
      (get-xy i y)))
  (define (get-col x)
    (for/list ([i (length grid)])
      (get-xy x i)))
  (define (seen? x y)
    (let ([val (get-xy x y)])
      (or (for/and ([i (take (get-row y) x)])
            (< i val))
          (for/and ([i (drop (get-row y) (add1 x))])
            (< i val))
          (for/and ([i (take (get-col x) y)])
            (< i val))
          (for/and ([i (drop (get-col x) (add1 y))])
            (< i val)))))
  (let* ([ys (length grid)]
         [xs (string-length (first grid))]
         [sum (+ ys ys xs xs -4)])
    (for* ([y (- ys 2)][x (- xs 2)])
      (when (seen? (add1 x) (add1 y))
        (set! sum (add1 sum))))
    (list xs ys sum)))

(define (part-A L)
  (~>> L
       visible-count))

(part-A test)
(part-A data)

;;---

(define count 0)

(define (most-trees grid)
  (define (get-xy x y)
    (~> grid
        (list-ref y)
        (string-ref x)
        string
        string->number))
  (define (get-row y)
    (for/list ([i (string-length (first grid))])
      (get-xy i y)))
  (define (get-col x)
    (for/list ([i (length grid)])
      (get-xy x i)))
  (define (count-smaller val lst)
    (define (iter acc lst*)
      (set! count (add1 count))
      ;(println (format "val: ~a, acc: ~a lst*: ~a" val acc lst*))
      (cond
        [(empty? lst*) acc]
        [(<= val (car lst*)) (add1 acc)]
        [else (iter (add1 acc) (rest lst*))]))
    (iter 0 lst))
  (let ([ys (length grid)]
        [xs (string-length (first grid))]
        [MAX 0])
    (for* ([y ys][x xs])
      ;(println (format "val(~a,~a): ~a" x y (get-xy x y)))
      (let ([m (* (count-smaller (get-xy x y) (reverse (take (get-row y) x)))
                  (count-smaller (get-xy x y) (drop (get-row y) (add1 x)))
                  (count-smaller (get-xy x y) (reverse (take (get-col x) y)))
                  (count-smaller (get-xy x y) (drop (get-col x) (add1 y))))])
        (when (< MAX m)
          (set! MAX m))))
    MAX))

(define (part-B L)
  (~>> L
       most-trees))

(part-B test)
(part-B data)
