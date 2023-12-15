#lang racket
(require racket/match)
(require threading)
(require racket/set)
(require (only-in "util.rkt" input->data tap point))

(define data (input->data "input/2023-15.txt"))

(define test0 "HASH")

(define test1 "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

;;

(define (hash-word str)
  (let* ([cs (string->list str)]
         [c-ints (map char->integer cs)]
         [h (λ (i acc) (modulo (* 17 (+ i acc)) 256))])
    (foldl h 0 c-ints)))
 
;; (hash-word test0)
   
(define (part-A L)
  (let ([words (string-split L ",")])
    (for/sum ([word words])
      (hash-word word))))

(part-A test1)
(part-A data)

;;

(define (part-B L)
  (let ([ops (string-split L ",")]
        [BOXS (make-hash)]
        [FOCALS (make-hash)])
    (define (equal-op op)
      (let* ([parts (string-split op "=")]
             [label (first parts)]
             [focal (second parts)]
             [box (hash-word label)])
        (hash-set! FOCALS label focal)
        (if (hash-has-key? BOXS box)
            (when (not (member label (hash-ref BOXS box)))
              (hash-set! BOXS box (flatten (list (hash-ref BOXS box) label))))
            (hash-set! BOXS box (list label)))
        ;(displayln (list op label focal box))
        ))
    (define (dash-op op)
      (let* ([op-len (string-length op)]
             [label (substring op 0 (sub1 op-len))]
             [box-num (hash-word label)])
        (when (hash-has-key? BOXS box-num)
          (hash-set! BOXS box-num (remove label (hash-ref BOXS box-num))))                
        ;(displayln (list op label box-num))
        ))
    (for ([op ops])
      (if (string-contains? op "=")
          (equal-op op)
          (dash-op op)))
    (for/sum ([i 256])
      (if (hash-has-key? BOXS i)
          (for/sum ([l (hash-ref BOXS i)][n (in-naturals)])
            (* (add1 i)
               (add1 n)
               (string->number (hash-ref FOCALS l))))
          0))
    ;(list BOXS FOCALS)
    ))

(part-B test1)
(part-B data)