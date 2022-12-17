#lang racket
(require threading)
(require graph)

(define data
  (~> "input/2022-17.txt"
      open-input-file
      (read-line _ 'return)
      ))

(define test ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

;;---
(struct point [x y] #:transparent)

(define (pt->str p)
  (match-let ([(point x y) p])
    (format "(~a,~a)" x y)))

(struct State [sh] #:transparent)

(define (st->str st)
  (match-let ([(State sh) st])
    (format "I:~a, R:~a, L: [~a]" (hash-ref index-HT 'index) (hash-ref round-HT 'round) (string-join (map pt->str sh) ","))))

(define point-HT (make-hash))
(define round-HT (make-hash))
(hash-set! round-HT 'round 0)
(define index-HT (make-hash))
(hash-set! index-HT 'index 0)

(for ([i 7])
  (hash-set! point-HT (point i 0) 1))

(define (get-max-h)
  (~>> point-HT
       hash->list
       (map car)
       (map point-y)
       (apply max)))

(define (dash h)
  (list (point 2 h)
        (point 3 h)
        (point 4 h)
        (point 5 h)))

(define (cross h)
  (list (point 3 h)
        (point 2 (add1 h))
        (point 3 (add1 h))
        (point 4 (add1 h))
        (point 3 (add1 (add1 h)))))

(define (corner h)
  (list (point 2 h)
        (point 3 h)
        (point 4 h)
        (point 4 (add1 h))
        (point 4 (add1 (add1 h)))))

(define (pipe h)
  (list (point 2 h)
        (point 2 (+ 1 h))
        (point 2 (+ 2 h))
        (point 2 (+ 3 h))))

(define (box h)
  (list (point 2 h)
        (point 3 h)
        (point 2 (add1 h))
        (point 3 (add1 h))))

(define (get-shape h)
  (let ([mod-count (modulo (hash-ref round-HT 'round) 5)])
    (cond
      [(= 0 mod-count) (dash h)]
      [(= 1 mod-count) (cross h)]
      [(= 2 mod-count) (corner h)]
      [(= 3 mod-count) (pipe h)]
      [(= 4 mod-count) (box h)])))

(define (left? sh)
  (for/and ([p sh])
    (match-let ([(point x y) p])
      (nor (zero? x)
           (hash-ref point-HT (point (sub1 x) y) #f)))))

(define (sh-left sh)
  (for/list ([p sh])
    (match-let ([(point x y) p])
      (point (sub1 x) y))))

(define (right? sh)
  (for/and ([p sh])
    (match-let ([(point x y) p])
      (nor (= 6 x)
           (hash-ref point-HT (point (add1 x) y) #f)))))

(define (sh-right sh)
  (for/list ([p sh])
    (match-let ([(point x y) p])
      (point (add1 x) y))))

(define (down? sh)
  (for/and ([p sh])
    (match-let ([(point x y) p])
      (not (hash-ref point-HT (point x (sub1 y)) #f)))))

(define (sh-down sh)
  (for/list ([p sh])
    (match-let ([(point x y) p])
      (point x (sub1 y)))))

(define (freeze sh)
  (for ([p sh])
    (hash-set! point-HT p 1)))

(define (right sh)
  (if (right? sh)
      (sh-right sh)
      sh))

(define (left sh)
  (if (left? sh)
      (sh-left sh)
      sh))

(define (down sh)
  (cond
    [(down? sh) (sh-down sh)]
    [else
     (freeze sh)
     (hash-set! round-HT 'round (add1 (hash-ref round-HT 'round)))
     (get-shape (+ 4 (get-max-h)))]))



;; tests
;(left? (list (point 0 1)))
;(hash-set! point-HT (point 1 1) 1)
;(left? (list (point 2 1)))
;(left? (list (point 4 4)))

;(right? (list (point 6 1)))
;(hash-set! point-HT (point 4 1) 1)
;(right? (list (point 3 1)))
;(right? (list (point 4 4)))

;(down? (list (point 6 1)))
;(hash-set! point-HT (point 2 0) 1)
;(down? (list (point 2 1)))
;(down? (list (point 4 4)))

(define (part-A L count-until)
  (define state (State (dash 4)))
  (define (update state)
    ;(println (st->str state))
    ;(print-state state)
    (match-let ([(State sh) state])
      (let ([dir (string-ref L (modulo (hash-ref index-HT 'index) (string-length L)))])
        (hash-set! index-HT 'index (add1 (hash-ref index-HT 'index)))
        ;(println (list 'dir dir))
        (define (next-shape sh dir)
          (if (char=? #\> dir)
              (down (right sh))
              (down (left sh))))
        (State (next-shape sh dir)))))

  (define (print-state st)
    (match-let ([(State sh) st])
      (define top (apply max (for/list ([p sh]) (point-y p))))
      (define fr (map car (hash->list point-HT)))
      (define (pt->str pt)
        (cond
          [(zero? (point-y pt)) "-"]
          [(member pt sh) "@"]
          [(member pt fr) "#"]
          [else "."]))
      (for ([r (range (add1 top) -1 -1)])
        (for ([c (range 7)])
          (let ([pt (point c r)])
            (display (pt->str pt))))
        (displayln ""))
      (displayln "")))

  (define (run st)
    (if (<= count-until (hash-ref round-HT 'round))
        (get-max-h)
        (run (update st))))
    

  (+ 4 (run state)))

;;---

;(part-A test 2020)
;(part-A data 2020)

(part-A test 1000000000000)