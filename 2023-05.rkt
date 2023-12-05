#lang racket
(require threading)
(require racket/match)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2023-05.txt"))

(define test "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

;;

(define (part-A L)
  (let* ([lines (string-split L "\n\n")]
         [seeds (map string->number (rest (string-split (first lines))))]
         [seed-to-soil (rest (string-split (second lines) "\n"))]
         [soil-to-fertilizer (rest (string-split (third lines) "\n"))]
         [fertilizer-to-water (rest (string-split (fourth lines) "\n"))]
         [water-to-light (rest (string-split (fifth lines) "\n"))]
         [light-to-temperature (rest (string-split (sixth lines) "\n"))]
         [temperature-to-humidity (rest (string-split (seventh lines) "\n"))]
         [humidity-to-location (rest (string-split (eighth lines) "\n"))])

    (define (lookup idx LoN)
      (or (for/or ([l LoN])
            (match-let ([(list dest src rng) (map string->number (string-split l))])
              (if (and (<= src idx) (< idx (+ src rng)))
                  (+ idx (- dest src))
                  #f)))
          idx))
    (apply min
           (for/list ([s seeds])
             (~> s
                 (lookup seed-to-soil)
                 (lookup soil-to-fertilizer)
                 (lookup fertilizer-to-water)
                 (lookup water-to-light)
                 (lookup light-to-temperature)
                 (lookup temperature-to-humidity)
                 (lookup humidity-to-location)
                 ))
           )))

;(part-A test)
;(part-A data)

;;



(define (part-B L)
  (let* ([lines (string-split L "\n\n")]
         [seeds (map string->number (rest (string-split (first lines))))]
         [seed-to-soil (rest (string-split (second lines) "\n"))]
         [soil-to-fertilizer (rest (string-split (third lines) "\n"))]
         [fertilizer-to-water (rest (string-split (fourth lines) "\n"))]
         [water-to-light (rest (string-split (fifth lines) "\n"))]
         [light-to-temperature (rest (string-split (sixth lines) "\n"))]
         [temperature-to-humidity (rest (string-split (seventh lines) "\n"))]
         [humidity-to-location (rest (string-split (eighth lines) "\n"))])

    (define (rev-lookup idx LoN)
      (or (for/or ([l LoN])
            (match-let ([(list dest src rng) (map string->number (string-split l))])
              (if (and (<= dest idx) (< idx (+ dest rng)))
                  (+ (- src dest) idx)
                  #f)))
          idx))

    (define (valid-seed? s)
      (define (iter L)
        (cond
          [(empty? L) #f]
          [(and (<= (first L) s) (< s (+ (first L) (second L)))) s]
          [else (iter (cddr L))]))
      (or (iter seeds) #f))
    
    (for/or ([loc 84470622])
      (when (zero? (modulo loc 1000000))
        (println (list (current-seconds) loc)))
      (define (check? x) (if (valid-seed? x) loc #f))
      (~> loc
          (rev-lookup humidity-to-location)
          (rev-lookup temperature-to-humidity)
          (rev-lookup light-to-temperature)
          (rev-lookup water-to-light)
          (rev-lookup fertilizer-to-water)
          (rev-lookup soil-to-fertilizer)
          (rev-lookup seed-to-soil)
          check?))
    
    ))

(part-B test)
(part-B data)
