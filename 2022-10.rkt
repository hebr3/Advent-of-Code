#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2022-10.txt"
      open-input-file
      (read-line _ 'return)
      ))

(define test0 "noop
addx 3
addx -5")

(define test "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

;;---

(define (run-ops List-Ops)
  (define (iter X acc L*)
    (cond
      [(empty? L*) (cons X acc)]
      [(string=? "noop" (first L*))
       (iter X (cons X acc) (rest L*))]
      [else
       (match-let ([(list _ num) (string-split (first L*) " ")])
         (iter (+ (string->number num) X) (cons X (cons X acc)) (rest L*)))]))
  (iter 1 '() List-Ops))

(define (take-20s L)
  (for/list ([i (floor (/ (+ 20 (length L)) 40))])
    (let ([M (+ -20 (* (add1 i) 40))])
      (* M (list-ref L (sub1 M))))))
    

(define (part-A L)
  (~>> L
       (string-split _ "\n")
       run-ops
       reverse
       take-20s
       (apply +)
       ))

;(part-A test0)
(part-A test)
(part-A data)

;;---

(define (build-screen LoN)
  (for/list ([row 6])
    (for/list ([cyc 40])
      (let ([N (list-ref LoN (+ cyc (* row 40)))])
        (if (<= (sub1 cyc) N (add1 cyc))
            "█"
            " ")))))

(define (part-B L)
  (~>> L
       (string-split _ "\n")
       run-ops
       reverse
       build-screen
       (map (λ (l) (string-join l "")))))

(part-B test)
(part-B data)
