#lang racket
(require 2htdp/image)
(require 2htdp/universe)

(struct pt [x y dx dy] #:transparent)

(define min -100)
(define max 100)

(define (string->pt str)
  (define rx #rx"position=<(.+),(.+)> velocity=<(.+),(.+)>")
  (match-let ([(list m x y dx dy) (regexp-match rx str)])
    (pt (string->number (string-trim x))
        (string->number (string-trim y))
        (string->number (string-trim dx))
        (string->number (string-trim dy)))))

(define (update-pt p)
  (let ([x (pt-x p)]
        [y (pt-y p)]
        [dx (pt-dx p)]
        [dy (pt-dy p)])
    (pt (wrap (+ x dx)) (wrap (+ y dy)) dx dy)))

(define (wrap num)
  (cond
    [(< num min) (+ num 200)]
    [(< max num) (- num 200)]
    [else num]))

(define (update-world wld)
  (map update-pt wld))

(define scene (empty-scene 200 200))

(define (render-pt p bkg)
  (place-image (rectangle 10 10 'solid 'black)
               (+ 100 (* 10 (pt-x p)))
               (+ 100 (* 10 (pt-y p)))
               bkg))

(define (render-world wld)
  (foldl (λ (pt bkg) (render-pt pt bkg))
         scene
         wld))

(define test
  (file->lines "test.txt"))

(define world
  (map string->pt test))

(big-bang world
  (on-tick update-world 1)
  (to-draw render-world))