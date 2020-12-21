#lang racket

(define input 277678)

;; Part 1 https://oeis.org/A214526

;1 0
;2 1
;3 2
;4 1
;5 2
;6 1
;7 2
;8 1
;9 2
;10 3
;11 2
;12 3
;13 4

(define (build-mapping-layer-N N)
  (for*/list ([i 4][j N])
    (cons i j)))

(build-mapping-layer-N 2)



;; Part 1 found on oeis
;; https://oeis.org/A141481