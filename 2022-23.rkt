#lang racket
(require threading)
(require plot)

(define data
  (~> "input/2022-23.txt"
      open-input-file
      (read-line _ 'return)
      ))

(define test "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..")

;;---

(struct point [x y] #:transparent)

(define (draw-board brd)
  (define vals (map car (hash->list brd)))

  (define xs (map point-x vals))
  (define ys (map point-y vals))
  
  (define x-min (apply min xs))
  (define x-max (apply max xs))
  (define y-min (apply min ys))
  (define y-max (apply max ys))
  
  (for ([y (in-range y-min (add1 y-max))])
    (for ([x (in-range x-min (add1 x-max))])
      (if (hash-has-key? brd (point x y))
          (display "#")
          (display ".")))
    (displayln "")))

(define (score-board brd)
  (define vals (map car (hash->list brd)))
  (define xs (map point-x vals))
  (define ys (map point-y vals))
  
  (define x-min (apply min xs))
  (define x-max (apply max xs))
  (define y-min (apply min ys))
  (define y-max (apply max ys))

  (for/sum ([y (in-range y-min (add1 y-max))])
    (for/sum ([x (in-range x-min (add1 x-max))])
      (if (hash-has-key? brd (point x y))
          0
          1))))

(define (part-A in)
  (define board-HT (make-hash))

  (define input (string-split in "\n"))
  (for ([row input][y (in-naturals)])
    (for ([c row][x (in-naturals)])
      (when (char=? c #\#)
        (hash-set! board-HT (point x y) #t))))
  
  (define (iter board-HT CNT)
    (define propose-HT (make-hash))

    (define (alone pt)
      (match-let ([(point x y) pt])
        (unless (or (hash-has-key? board-HT (point (sub1 x) (add1 y)))
                    (hash-has-key? board-HT (point x (add1 y)))
                    (hash-has-key? board-HT (point (add1 x) (add1 y)))
                    (hash-has-key? board-HT (point (add1 x) y))
                    (hash-has-key? board-HT (point (add1 x) (sub1 y)))
                    (hash-has-key? board-HT (point x (sub1 y)))
                    (hash-has-key? board-HT (point (sub1 x) (sub1 y)))
                    (hash-has-key? board-HT (point (sub1 x) y)))
          (hash-set! board-HT pt 'moved)
          (hash-set! propose-HT pt (list pt)))))
    
    (define (north pt)
      (when (not (equal? 'moved (hash-ref board-HT pt)))
        (match-let ([(point x y) pt])
          (unless (or (hash-has-key? board-HT (point (sub1 x) (sub1 y)))
                      (hash-has-key? board-HT (point x (sub1 y)))
                      (hash-has-key? board-HT (point (add1 x) (sub1 y))))
            (let ([old (hash-ref propose-HT (point x (sub1 y)) '())])
              ;(displayln (list 'north pt))
              (hash-set! board-HT pt 'moved)
              (hash-set! propose-HT (point x (sub1 y)) (cons pt old)))))))
    
    (define (south pt)
      (when (not (equal? 'moved (hash-ref board-HT pt)))
        (match-let ([(point x y) pt])
          (unless (or (hash-has-key? board-HT (point (sub1 x) (add1 y)))
                      (hash-has-key? board-HT (point x (add1 y)))
                      (hash-has-key? board-HT (point (add1 x) (add1 y))))
            (let ([old (hash-ref propose-HT (point x (add1 y)) '())])
              ;(displayln (list 'south pt))
              (hash-set! board-HT pt 'moved)
              (hash-set! propose-HT (point x (add1 y)) (cons pt old)))))))
    
    (define (west pt)
      (when (not (equal? 'moved (hash-ref board-HT pt)))
        (match-let ([(point x y) pt])
          (unless (or (hash-has-key? board-HT (point (sub1 x) (add1 y)))
                      (hash-has-key? board-HT (point (sub1 x) y))
                      (hash-has-key? board-HT (point (sub1 x) (sub1 y))))
            (let ([old (hash-ref propose-HT (point (sub1 x) y) '())])
              ;(displayln (list 'west pt))
              (hash-set! board-HT pt 'moved)
              (hash-set! propose-HT (point (sub1 x) y) (cons pt old)))))))
    
    (define (east pt)
      (when (not (equal? 'moved (hash-ref board-HT pt)))
        (match-let ([(point x y) pt])
          (unless (or (hash-has-key? board-HT (point (add1 x) (add1 y)))
                      (hash-has-key? board-HT (point (add1 x) y))
                      (hash-has-key? board-HT (point (add1 x) (sub1 y))))
            (let ([old (hash-ref propose-HT (point (add1 x) y) '())])
              ;(displayln (list 'east pt))
              (hash-set! board-HT pt 'moved)
              (hash-set! propose-HT (point (add1 x) y) (cons pt old)))))))

    (define (not-move pt)
      (when (not (equal? 'moved (hash-ref board-HT pt)))
        ;(displayln (list 'not-move pt))
        (hash-set! propose-HT pt (list pt))))
    
    (for ([pt (hash-keys board-HT)])
      (alone pt)
      (cond
        [(= 0 (modulo CNT 4))
         (north pt)
         (south pt)
         (west pt)
         (east pt)]
        [(= 1 (modulo CNT 4))
         (south pt)
         (west pt)
         (east pt)
         (north pt)]
        [(= 2 (modulo CNT 4))
         (west pt)
         (east pt)
         (north pt)
         (south pt)]
        [(= 3 (modulo CNT 4))
         (east pt)
         (north pt)
         (south pt)
         (west pt)])
      (not-move pt))

    (define next-HT (make-hash))

    (for ([pt (hash-keys propose-HT)])
      (let ([prev (hash-ref propose-HT pt)])
        (cond
          [(= 1 (length prev))
           (hash-set! next-HT pt #t)]
          [else
           (for ([pt prev]) (hash-set! next-HT pt #t))])))
    next-HT)

  (let ([BOARD board-HT])
    (for ([i 1000])
      (define next (iter BOARD i))
      (define old-vals (apply set (map car (hash->list BOARD))))
      (define new-vals (apply set (map car (hash->list next))))
      (when (set=? old-vals new-vals)
        (displayln (list 'equals i)))
      ;(displayln old-vals)
      ;(draw-board next)
      ;(displayln "")
      (set! BOARD next))
    (score-board BOARD)))


;;---

;(part-A test)
(part-A data)
