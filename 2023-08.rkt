#lang racket
(require threading)
(require racket/match)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2023-08.txt"))

(define test "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

;;

(define (part-A L)
  (define LOOKUP-HASH (make-hash))
  (match-let* ([(list lr-str connection-str) (string-split L "\n\n")])
    (let* ([list-of-connection-strs (string-split connection-str "\n")])
      (for ([conn list-of-connection-strs])
        (match-let ([(list _ vert l r) (regexp-match #px"(...) = \\((...), (...)" conn)])
          (hash-set! LOOKUP-HASH vert (list l r)))))
    (define (iter v c)
      (cond
        [(string=? "ZZZ" v) c]
        [(char=? #\R (string-ref lr-str (modulo c (string-length lr-str))))
         (iter (second (hash-ref LOOKUP-HASH v)) (add1 c))]
        [else
         (iter (first (hash-ref LOOKUP-HASH v)) (add1 c))]))
    (iter "AAA" 0)))

(part-A test)
(part-A data)

;;

(define test2 "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(define (a? str)
  (char=? #\A (string-ref str 2)))

(define (z? str)
  (char=? #\Z (string-ref str 2)))

(define (all-z? los)
  (for/and ([s los])
    (z? s)))

(define (part-B L)
  (define LOOKUP-HASH (make-hash))
  (match-let* ([(list lr-str connection-str) (string-split L "\n\n")])
    (let* ([list-of-connection-strs (string-split connection-str "\n")])
      (for ([conn list-of-connection-strs])
        (match-let ([(list _ vert l r) (regexp-match #px"(...) = \\((...), (...)" conn)])
          (hash-set! LOOKUP-HASH vert (list l r))))
      (define a-ends (filter a? (map (λ (s) (first (string-split s " ="))) list-of-connection-strs)))
;;       (define (iter vs c)
;;         (when (zero? (modulo c 10000))
;;           (println c))
;;         (cond
;;           [(all-z? vs) c]
;;           [(char=? #\R (string-ref lr-str (modulo c (string-length lr-str))))
;;            (iter (for/list ([v vs]) (second (hash-ref LOOKUP-HASH v))) (add1 c))]
;;           [else
;;            (iter (for/list ([v vs]) (first (hash-ref LOOKUP-HASH v))) (add1 c))]))
      ;(iter a-ends 0)
      (define (iter v c)
        (cond
          [(z? v) c]
          [(char=? #\R (string-ref lr-str (modulo c (string-length lr-str))))
           (iter (second (hash-ref LOOKUP-HASH v)) (add1 c))]
          [else
           (iter (first (hash-ref LOOKUP-HASH v)) (add1 c))]))
      (apply lcm (for/list ([a a-ends])
                   (iter a 0))))))
          
(part-B test2)
(part-B data)
