#lang racket
(require threading)

(define test-input
  '("pbga (66)"
    "xhth (57)"
    "ebii (61)"
    "havc (66)"
    "ktlj (57)"
    "fwft (72) -> ktlj, cntj, xhth"
    "qoyq (66)"
    "padx (45) -> pbga, havc, qoyq"
    "tknk (41) -> ugml, padx, fwft"
    "jptl (61)"
    "ugml (68) -> gyxo, ebii, jptl"
    "gyxo (61)"
    "cntj (57)"))

(define input
;  (~> test-input
;      (map string-split _)))
  (~> "input.txt"
      file->lines
      (map string-split _)))

(define programs
    (make-hash (for/list ([i input])
                 (cons (car i) (cadr i)))))

(define programs*
  (for/set ([(k v) programs]) k))

(define towers
  (~> input
      (filter (λ (x) (< 2 (length x))) _)
      (map (λ (x) (drop x 3)) _)
      (apply append _)
      (map (λ (x) (string-trim x ",")) _)
      remove-duplicates
      (apply set _)))

(set-subtract programs* towers)

(define sub-towers
  (~> input
      (filter (λ (x) (< 3 (length x))) _)
      (map (λ (x) (drop x 3)) _)
      (map (λ (x) (map (λ (s) (string-trim s ",")) x)) _)))

(map (λ (st) (map (λ (t) (hash-ref programs t)) st))
     sub-towers)
