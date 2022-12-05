#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2022-05.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")
      ))

(define test '("    [D]    "
               "[N] [C]    "
               "[Z] [M] [P]"
               " 1   2   3 "
               ""
               "move 1 from 2 to 1"
               "move 3 from 1 to 3"
               "move 2 from 2 to 1"
               "move 1 from 1 to 2"))

(define init '(("N" "Q" "L" "S" "C" "Z" "P" "T")
               ("G" "C" "H" "V" "T" "P" "L")
               ("F" "Z" "C" "D")
               ("C" "V" "M" "L" "D" "T" "W" "G")
               ("C" "W" "P")
               ("Z" "S" "T" "C" "D" "J" "F" "P")
               ("D" "B" "G" "W" "V")
               ("W" "H" "Q" "S" "J" "N")
               ("V" "L" "S" "F" "Q" "C" "R")))

;;---

(define (add-or-remove L val cnt from to col)
  (cond
    [(= from col) (drop L 1)]
    [(= to col) (flatten (cons val L))]
    [else L]))

(define (parse-move mv state)
  (define (iter cnt from to s*)
    (match-let ([(list A B C D E F G H I) s*])
      (if (zero? cnt)
          s*
          (let ([val (car (list-ref s* (sub1 from)))])
            (iter (sub1 cnt)
                  from
                  to
                  (list (add-or-remove A val cnt from to 1)
                        (add-or-remove B val cnt from to 2)
                        (add-or-remove C val cnt from to 3)
                        (add-or-remove D val cnt from to 4)
                        (add-or-remove E val cnt from to 5)
                        (add-or-remove F val cnt from to 6)
                        (add-or-remove G val cnt from to 7)
                        (add-or-remove H val cnt from to 8)
                        (add-or-remove I val cnt from to 9)))))))
  (match-let ([(list _ cnt _ i _ f) (string-split mv " ")])
    (iter (string->number cnt) (string->number i) (string->number f) state)))
  
(define (part-A L)
  (~>> 42))

(foldl parse-move init (drop data 10))

(part-A test)
(part-A data)

;;---

(define (add-or-remove2 L cnt val from to col)
  (cond
    [(= from col) (drop L cnt)]
    [(= to col) (flatten (cons val L))]
    [else L]))

(define (parse-move2 mv state)
  (define (iter cnt from to s*)
    (match-let ([(list A B C D E F G H I) s*])
      (let ([val (take (list-ref s* (sub1 from)) cnt)])
        (list (add-or-remove2 A cnt val from to 1)
              (add-or-remove2 B cnt val from to 2)
              (add-or-remove2 C cnt val from to 3)
              (add-or-remove2 D cnt val from to 4)
              (add-or-remove2 E cnt val from to 5)
              (add-or-remove2 F cnt val from to 6)
              (add-or-remove2 G cnt val from to 7)
              (add-or-remove2 H cnt val from to 8)
              (add-or-remove2 I cnt val from to 9)))))
  (match-let ([(list _ cnt _ i _ f) (string-split mv " ")])
    (iter (string->number cnt) (string->number i) (string->number f) state)))

(foldl parse-move2 init (drop data 10))

(define (part-B L)
  (~>> 42))

(part-B test)
(part-B data)
