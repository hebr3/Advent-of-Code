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

    (define (hash-ref* hs idx)
      (if (hash-has-key? hs idx)
          (hash-ref hs idx)
          idx))
    
    (define seed-to-soil-hash (make-hash))
    (for ([m seed-to-soil])
      (match-let ([(list dest src rng) (map string->number (string-split m))])
        (for ([i rng])
          (hash-set! seed-to-soil-hash (+ src i) (+ dest i)))))
    (println 'one)
    
    (define soil-to-fertilizer-hash (make-hash))
    (for ([m soil-to-fertilizer])
      (match-let ([(list dest src rng) (map string->number (string-split m))])
        (for ([i rng])
          (hash-set! soil-to-fertilizer-hash (+ src i) (+ dest i)))))
    (println 'two)
    
    (define fertilizer-to-water-hash (make-hash))
    (for ([m fertilizer-to-water])
      (match-let ([(list dest src rng) (map string->number (string-split m))])
        (for ([i rng])
          (hash-set! fertilizer-to-water-hash (+ src i) (+ dest i)))))
    (println 'three)
    
    (define water-to-light-hash (make-hash))
    (for ([m water-to-light])
      (match-let ([(list dest src rng) (map string->number (string-split m))])
        (for ([i rng])
          (hash-set! water-to-light-hash (+ src i) (+ dest i)))))
    (println 'four)
    
    (define light-to-temperature-hash (make-hash))
    (for ([m light-to-temperature])
      (match-let ([(list dest src rng) (map string->number (string-split m))])
        (for ([i rng])
          (hash-set! light-to-temperature-hash (+ src i) (+ dest i)))))
    (println 'five)
    
    (define temperature-to-humidity-hash (make-hash))
    (for ([m temperature-to-humidity])
      (match-let ([(list dest src rng) (map string->number (string-split m))])
        (for ([i rng])
          (hash-set! temperature-to-humidity-hash (+ src i) (+ dest i)))))
    (println 'six)
    
    (define humidity-to-location-hash (make-hash))
    (for ([m humidity-to-location])
      (match-let ([(list dest src rng) (map string->number (string-split m))])
        (for ([i rng])
          (hash-set! humidity-to-location-hash (+ src i) (+ dest i)))))
    (println 'seven)
    
    (apply min
     (for/list ([s seeds])
       (~> s
           (hash-ref* seed-to-soil-hash _)
           (hash-ref* soil-to-fertilizer-hash _)
           (hash-ref* fertilizer-to-water-hash _)
           (hash-ref* water-to-light-hash _)
           (hash-ref* light-to-temperature-hash _)
           (hash-ref* temperature-to-humidity-hash _)
           (hash-ref* humidity-to-location-hash _)
           ))
     )))

(part-A test)
;(part-A data)

;;

(define (part-B L)
  L)

;(part-B test)
;(part-B data)
