#lang racket
(require threading)
(require racket/match)
(require lens)

(define data
  (~> "input/2021-12.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")))

(define test
  '("NNCB"
    ""
    "CH -> B"
    "HH -> N"
    "CB -> H"
    "NH -> C"
    "HB -> C"
    "HC -> B"
    "HN -> C"
    "NN -> C"
    "BH -> H"
    "NC -> B"
    "NB -> B"
    "BN -> B"
    "BB -> N"
    "BC -> B"
    "CC -> N"
    "CN -> C"))

(define (build-pair-table LoS)
  (define HASH (make-hash))
  (for ([str LoS])
    (match-let ([(list k v) (string-split str " -> ")])
      (hash-set! HASH k v)))
  HASH)

(define (format-data data)
  (list (car data) (build-pair-table (cddr data))))

(define (update-string formated-data)
  (match-let ([(list str hsh) formated-data])
    (~> (for/list ([i (sub1 (string-length str))])
          (let ([a (string-ref str i)]
                [b (string-ref str (add1 i))])
            (list (string a)
                  (hash-ref hsh (apply string (list a b))))))
        (append _ (list (substring str (sub1 (string-length str)))))
        flatten
        (string-join _ "")
        )))

(define (update-N-times formated-data N)
  (match-let ([(list str hsh) formated-data])
    (if (zero? N)
        str
        (update-N-times (list (update-string formated-data) hsh) (sub1 N)))))

(define (Max-minus-Min L)
  (- (apply max L) (apply min L)))

(define (calculate-Max-Min str)
  (~> str
      (string-split _ "")
      (filter non-empty-string? _)
      (group-by (λ (x) x) _)
      (map length _)
      Max-minus-Min))

(~> test
    format-data
    (update-N-times _ 10)
    (calculate-Max-Min _))