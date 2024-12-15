#lang racket/base
(require racket/string)
(provide string->matrix
         matrix-ref
         matrix-rotate
         display-matrix)

(define (string->matrix str)
  (for/list ([row (string-split str "\n")])
    (for/list ([ch row])
      ch)))

(define (matrix-ref mtx r c)
  (list-ref (list-ref mtx r) c))

(define (rotate-90 mat)
  (for/list ([col (length (car mat))])
    (reverse (for/list ([row (length mat)])
               (matrix-ref mat row col)))))

(define (matrix-rotate mat deg)
  (cond
    [(and (list? mat) (list? (car mat)) (zero? (modulo deg 90)))
     (define (iter mat* idx)
       (if (zero? idx)
           mat*
           (iter (rotate-90 mat*) (sub1 idx))))
     (iter mat (quotient deg 90))]
    [else
     (error "Arguments must be a matrix and a multiple of 90")]))

(define (display-matrix mat)
  (if (and (list? mat) (list? (car mat)))
    (for ([row mat])
      (for ([col row])
        (display col))
      (displayln ""))
    (error "Argument must be a matrix")))