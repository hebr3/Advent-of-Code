#lang racket
(require rackunit)
(require threading)

(define HS (make-hash))

(struct Orbit [A B] #:transparent)

(define (string->Orbit str)
  (let* ([vals (string-split str ")")]
         [A (first vals)]
         [B (second vals)])
    (Orbit A B)))

(define (Orbit->hash rel)
  (let ([B (hash-ref HS (Orbit-A rel) '())])
    (hash-set! HS (Orbit-A rel) (cons (Orbit-B rel) B))))

(struct Tree [val nodes] #:transparent)


(define (read-input file)
  (~>> file
       file->lines
       (map string->Orbit)))

(define test1 (read-input "test1.txt"))
(define input (read-input "input.txt"))



(define (Day5 in)
  (let ([count 0]
        [As (map Orbit-A in)]
        [Bs (map Orbit-B in)])
    (define (find-Bs val depth)
      (let* ([indexes-of-vals (indexes-of As val)]
             [list-of-Bs (map (λ (i) (list-ref Bs i)) indexes-of-vals)])
        (set! count (+ count depth))
        (if (empty? list-of-Bs)
            (Tree val list-of-Bs)
            (Tree val (map (λ (x) (find-Bs x (add1 depth))) list-of-Bs)))))
    (find-Bs "COM" 0)
    count))

(check-equal? (Day5 test1) 42 "Test Day5")
(check-equal? (Day5 input) 223251 "Test input")

(define test2 (read-input "test2.txt"))