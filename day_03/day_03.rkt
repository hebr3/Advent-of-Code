#lang racket
(require racket/generator)

(define spiral
  (let ([n 1][x 0][y 0][step 1][sub-step 1])
    (infinite-generator
     (yield (list n x y))
     (let loop ([i step])
       (cond
         [(odd? i)
          (for ([i* i])
            (set! x (add1 x))
            (set! n (add1 n))
            (yield (list n x y)))
          (for ([i* i])
            (set! y (add1 y))
            (set! n (add1 n))
            (yield (list n x y)))]
         [else
          (for ([i* i])
            (set! x (sub1 x))
            (set! n (add1 n))
            (yield (list n x y)))
          (for ([i* i])
            (set! y (sub1 y))
            (set! n (add1 n))
            (yield (list n x y)))])
       (set! step (add1 step))
       (loop step)))))

(last
 (for/list ([i 277678])
   (spiral)))