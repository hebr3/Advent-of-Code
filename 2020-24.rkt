#lang racket
(require threading)
(require racket/match)
(require rackunit)

;; Prep
(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)
      (string-split "\n")))

(define (test->list-of-strings str)
  (~> str
      (string-split "\n")))

;; Struct
(struct cord [x y] #:transparent)

;; Functions
(define (parse-dirs dirs)
  (define (NorS? ch)
    (or (char=? #\n ch) (char=? #\s ch)))
  (define (iter acc list-of-dir)
    (cond
      [(empty? list-of-dir)
       acc]
      [(NorS? (first list-of-dir))
       (iter (cons (string (first list-of-dir) (second list-of-dir)) acc) (rest (rest list-of-dir)))]
      [else
       (iter (cons (string (first list-of-dir)) acc) (rest list-of-dir))]))
  (reverse (iter '() dirs)))

(define (update-cord dir crd)
  (match-let ([(cord x y) crd])
    (match dir
      ["e" (cord (add1 x) y)]
      ["ne" (cord x (add1 y))]
      ["nw" (cord (sub1 x) (add1 y))]
      ["w" (cord (sub1 x) y)]
      ["sw" (cord x (sub1 y))]
      ["se" (cord (add1 x) (sub1 y))])))
      
(define (read-list list-of-dir)
  (foldl update-cord (cord 0 0) list-of-dir))

(define (update-grid list-of-cords)
  (define H (make-hash))
  (for ([c list-of-cords])
    (if (hash-ref H c #f)
        (hash-set! H c (not (hash-ref H c)))
        (hash-set! H c #t)))
  H)

(define (get-neighbors tile)
  (match-let ([(cord x y) tile])
    (let ([e (cord (add1 x) y)]
          [ne (cord x (add1 y))]
          [nw (cord (sub1 x) (add1 y))]
          [w (cord (sub1 x) y)]
          [sw (cord x (sub1 y))]
          [se (cord (add1 x) (sub1 y))])
      (list e ne nw w sw se))))
            
(define (live? tile list-of-cords)
  (let ([C (for/sum ([i (get-neighbors tile)])
             (if (member i list-of-cords) 1 0))])
    (if (member tile list-of-cords)
        (<= 1 C 2)
        (= C 2))))       

(define (update-list list-of-cords)
  (define H (make-hash))
  (for ([tile list-of-cords])
    (for ([neighbor (get-neighbors tile)])
      (when (live? neighbor list-of-cords)
        (hash-set! H neighbor #t))))
  (~>> H
       hash->list
       (map car)))

(define (update-list-n-times list-of-cords n)
  (if (zero? n)
      list-of-cords
      (update-list-n-times (update-list list-of-cords) (sub1 n))))

;; Data
(define data
  (~>> "input/2020-24.txt"
       (file->list-of-strings)))

(define test
  (~>> "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew
"
       test->list-of-strings))


;; Puzzle
(display "test 1: ")
(~>> test
     (map string->list)
     (map parse-dirs)
     (map read-list)
     update-grid
     hash->list
     (filter (λ (x) (cdr x)))
     length)

(display "one: ")
(~>> data
     (map string->list)
     (map parse-dirs)
     (map read-list)
     update-grid
     hash->list
     (filter (λ (x) (cdr x)))
     length)


(display "test 2: ")
(time
 (~>> test
      (map string->list)
      (map parse-dirs)
      (map read-list)
      update-grid
      hash->list
      (filter (λ (x) (cdr x)))
      (map car)
      (update-list-n-times _ 10)
      length))
(time
 (~>> test
      (map string->list)
      (map parse-dirs)
      (map read-list)
      update-grid
      hash->list
      (filter (λ (x) (cdr x)))
      (map car)
      (update-list-n-times _ 100)
      length))

;
(display "two: ")
(time
 (~>> data
      (map string->list)
      (map parse-dirs)
      (map read-list)
      update-grid
      hash->list
      (filter (λ (x) (cdr x)))
      (map car)
      (update-list-n-times _ 100)
      length))
;; 15min

;; Unit Test
(check-equal? (parse-dirs '(#\e #\s #\e #\s #\w #\w #\n #\w #\n #\e))
              (list "e" "se" "sw" "w" "nw" "ne"))

(check-equal? (update-cord "e" (cord 0 0))
              (cord 1 0))

(check-equal? (read-list '("e" "se"))
              (cord 2 -1))

(check-equal? (get-neighbors (cord 0 0))
              (list (cord 1 0) (cord 0 1) (cord -1 1) (cord -1 0) (cord 0 -1) (cord 1 -1)))
(check-equal? (get-neighbors (cord 2 -3))
              (list (cord 3 -3) (cord 2 -2) (cord 1 -2) (cord 1 -3) (cord 2 -4) (cord 3 -4)))

