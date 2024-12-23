#lang racket
(require threading)
(require 2htdp/image)

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

(define data (input->data "input/2024-14.txt"))

(define test0 "p=2,4 v=2,-3
")

(define test "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
")

(struct robot [ix iy dx dy] #:transparent)

(define (string->robot str)
  (match-let ([(list ix iy dx dy)
               (map string->number (regexp-match* #px"-*\\d+" str))])
    (robot ix iy dx dy)))

(define (part-A L X-MAX Y-MAX)
  (define lines (string-split L "\n"))

  (define (update-robot r)
    (match-let ([(robot ix iy dx dy) r])
      (let ([new-x (modulo (+ ix dx) X-MAX)]
            [new-y (modulo (+ iy dy) Y-MAX)])
        (robot new-x new-y dx dy))))
  
  (define robots (map string->robot lines))
  (define (iter lor n)
    (if (zero? n)
        lor
        (iter (map update-robot lor) (sub1 n))))

  (define (print-robots lor)
    (for ([y Y-MAX])
      (for ([x X-MAX])
        (let ([sum (for/sum ([r lor])
                     (match-let ([(robot ix iy _ _) r])
                       (if (and (= ix x) (= iy y))
                           1 0)))])
          (if (zero? sum)
              (display ".")
              (display sum))))
      (displayln ""))
    (displayln ""))

  (define (quadrant lor)
    (let ([A 0][B 0][C 0][D 0])
      (for ([r lor])
        (match-let ([(robot ix iy _ _) r])
          (cond
            [(and (<= 0 ix 49) (<= 0 iy 50))
             (set! A (add1 A))]
            [(and (<= 51 ix X-MAX) (<= 0 iy 50))
             (set! B (add1 B))]
            [(and (<= 0 ix 49) (<= 52 iy Y-MAX))
             (set! C (add1 C))]
            [(and (<= 51 ix X-MAX) (<= 52 iy Y-MAX))
             (set! D (add1 D))])))
      (* A B C D)))

  (quadrant (iter robots 100)))

;(part-A test 11 7)
(part-A data 101 103)

;;

(define points-of-interest (flatten (for/list ([i 100]) (list (+ (* i 103) 25) (+ (* i 101) 62)))))
(define vertical-poi (for/list ([i (list 68 69 70 71 72)]) (+ (* i 101) 62)))

(define (part-B L X-MAX Y-MAX)
  (define lines (string-split L "\n"))

  (define (update-robot r)
    (match-let ([(robot ix iy dx dy) r])
      (let ([new-x (modulo (+ ix dx) X-MAX)]
            [new-y (modulo (+ iy dy) Y-MAX)])
        (robot new-x new-y dx dy))))
  
  (define robots (map string->robot lines))
  (define (iter lor n)
    (if (zero? n)
        lor
        (iter (map update-robot lor) (sub1 n))))

  (define (draw-lor lor)
    (define background (empty-scene X-MAX Y-MAX))
    (for ([r lor])
      (match-let ([(robot ix iy dx dy) r])
        (set! background
              (place-image (square 1 'solid 'black) ix iy background))))
    background)
  
  (for/list ([i vertical-poi])
    (list i (draw-lor (iter robots i)))))

;(part-B test)
(part-B data 101 103)

