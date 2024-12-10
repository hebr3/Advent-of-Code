#lang racket
(require threading)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2024-10.txt"))

(define test "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(struct Loc [height x y] #:transparent)

(define (char->number c)
  (string->number (string c)))

(define (part-A L)
  (define lines (string-split L "\n"))
  (define locations (for/list ([line lines][r (in-naturals)])
                      (for/list ([ch line][c (in-naturals)])
                        (Loc (char->number ch) c r))))

  (define loc-dag (make-hash))

  (define (hash-add! ht k v)
    (if (hash-has-key? ht k)
        (hash-set! ht k (cons v (hash-ref ht k)))
        (hash-set! ht k (list v))))
  
  (for ([i (in-range (sub1 (length locations)))])
    (for ([a (list-ref locations i)]
          [b (list-ref locations (add1 i))])
      (match-let ([(Loc ha xa ya) a]
                  [(Loc hb xb yb) b])
        (cond
          [(= (sub1 ha) hb) (hash-add! loc-dag b a)]
          [(= (add1 ha) hb) (hash-add! loc-dag a b)]
          [else 'more-than-two]))))

  (for ([row locations])
    (for ([a row][b (rest row)])
      (match-let ([(Loc ha xa ya) a]
                  [(Loc hb xb yb) b])
        (cond
          [(= (sub1 ha) hb) (hash-add! loc-dag b a)]
          [(= (add1 ha) hb) (hash-add! loc-dag a b)]
          [else 'more-than-two]))))

  (define (walk-trail start)
    (hash-ref loc-dag start #f))
  (define (move-up-one l)
    (walk-trail l))
  (define (ff l)
    (remove-duplicates
     (flatten (map walk-trail (filter (λ (x) x) l)))))

  (define (F s)
    (ff ;9
     (ff ;8
      (ff ;7
       (ff ;6
        (ff ;5
         (ff ;4
          (ff ;3
           (ff ;2
            (walk-trail s))))))))))

  (define (sum l)
    (apply + l))

  (sum (map length (map F (filter (λ (x) (zero? (Loc-height x)))
                                  (flatten locations))))))


(part-A test)
(part-A data)

;;

(define (part-B L)
  (define lines (string-split L "\n"))
  (define locations (for/list ([line lines][r (in-naturals)])
                      (for/list ([ch line][c (in-naturals)])
                        (Loc (char->number ch) c r))))

  (define loc-dag (make-hash))

  (define (hash-add! ht k v)
    (if (hash-has-key? ht k)
        (hash-set! ht k (cons v (hash-ref ht k)))
        (hash-set! ht k (list v))))
  
  (for ([i (in-range (sub1 (length locations)))])
    (for ([a (list-ref locations i)]
          [b (list-ref locations (add1 i))])
      (match-let ([(Loc ha xa ya) a]
                  [(Loc hb xb yb) b])
        (cond
          [(= (sub1 ha) hb) (hash-add! loc-dag b a)]
          [(= (add1 ha) hb) (hash-add! loc-dag a b)]
          [else 'more-than-two]))))

  (for ([row locations])
    (for ([a row][b (rest row)])
      (match-let ([(Loc ha xa ya) a]
                  [(Loc hb xb yb) b])
        (cond
          [(= (sub1 ha) hb) (hash-add! loc-dag b a)]
          [(= (add1 ha) hb) (hash-add! loc-dag a b)]
          [else 'more-than-two]))))

  (define (walk-trail start)
    (hash-ref loc-dag start #f))
  (define (move-up-one l)
    (walk-trail l))
  (define (ff l)
    (flatten (map walk-trail (filter (λ (x) x) l))))

  (define (F s)
    (ff ;9
     (ff ;8
      (ff ;7
       (ff ;6
        (ff ;5
         (ff ;4
          (ff ;3
           (ff ;2
            (walk-trail s))))))))))

  (define (sum l)
    (apply + l))

  (sum (map length (map F (filter (λ (x) (zero? (Loc-height x)))
                                  (flatten locations))))))

(part-B test)
(part-B data)
