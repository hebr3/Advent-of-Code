#lang racket
(require threading)
(require plot)

(define data
  (~> "input/2022-21.txt"
      open-input-file
      (read-line _ 'return)
      ))

(define test "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32")

;;---

(define (string->instruction str)
  (string-split str #px": | "))

(define (part-A in)
  (define lookup-HT (make-hash))
  (define input (string-split in "\n"))
  (define inst (map string->instruction input))
  (for ([i inst])
    (hash-set! lookup-HT (first i) (rest i)))

  (define (run-inst inst)
    ;(println inst)
    (let ([calc (hash-ref lookup-HT inst #f)])
      ;(println calc)
      (cond
        [(member "+" calc)
         (+ (run-inst (first calc))
            (run-inst (third calc)))]
        [(member "-" calc)
         (- (run-inst (first calc))
            (run-inst (third calc)))]
        [(member "/" calc)
         (/ (run-inst (first calc))
            (run-inst (third calc)))]
        [(member "*" calc)
         (* (run-inst (first calc))
            (run-inst (third calc)))]
        [(string->number (first calc))
         (string->number (first calc))]
        [else
         (run-inst (first calc))])))
  (run-inst "root"))

;;---

(define (part-B in)
  (define lookup-HT (make-hash))
  (define input (string-split in "\n"))
  (define inst (map string->instruction input))
  (for ([i inst])
    (hash-set! lookup-HT (first i) (rest i)))

  (define (run-inst inst)
    ;(println inst)
    (let ([calc (hash-ref lookup-HT inst #f)])
      ;(println calc)
      (cond
        [(member "+" calc)
         (+ (run-inst (first calc))
            (run-inst (third calc)))]
        [(member "-" calc)
         (- (run-inst (first calc))
            (run-inst (third calc)))]
        [(member "/" calc)
         (/ (run-inst (first calc))
            (run-inst (third calc)))]
        [(member "*" calc)
         (* (run-inst (first calc))
            (run-inst (third calc)))]
        [(string->number (first calc))
         (string->number (first calc))]
        [else
         (run-inst (first calc))])))

  (define (run-init-iter humn)
    (match-define (list a _ b) (hash-ref lookup-HT "root"))
    (hash-set! lookup-HT "root" (list a "-" b))
    (hash-set! lookup-HT "humn" (list (number->string humn)))
    (run-inst "root"))
  
  (define (part2 low high)
    (define mid (+ (floor (/ (- high low) 2)) low))
    (define low* (run-init-iter low))
    (define mid* (run-init-iter mid))
    (define high* (run-init-iter high))
    (println (list low mid high))
    (cond
      [(zero? mid*) mid]
      [(< mid* 0)
       (part2 low mid)]
      [else
       (part2 mid high)]))
  
  (part2 -10000000000000 10000000000000))
;  (for ([i (range 100)])
;    (define i* (+ 3560324848100 i))
;    (hash-set! lookup-HT "humn" (list (number->string i*)))
;    (match-let ([(list a _ b) (hash-ref lookup-HT "root")])
;      (when (= (run-inst a) (run-inst b))
;        (println i*)))))

;;---

;(part-A test)
;(part-A data)
;(part-B test)
(part-B data)

