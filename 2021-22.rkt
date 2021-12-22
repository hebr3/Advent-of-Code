#lang racket
(require threading)
(require racket/match)
(require lens)
(require pmap)

(define data
  (~> "input/2021-22.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")))

(define test
  '("on x=-20..26,y=-36..17,z=-47..7"
    "on x=-20..33,y=-21..23,z=-26..28"
    "on x=-22..28,y=-29..23,z=-38..16"
    "on x=-46..7,y=-6..46,z=-50..-1"
    "on x=-49..1,y=-3..46,z=-24..28"
    "on x=2..47,y=-22..22,z=-23..27"
    "on x=-27..23,y=-28..26,z=-21..29"
    "on x=-39..5,y=-6..47,z=-3..44"
    "on x=-30..21,y=-8..43,z=-13..34"
    "on x=-22..26,y=-27..20,z=-29..19"
    "off x=-48..-32,y=26..41,z=-47..-37"
    "on x=-12..35,y=6..50,z=-50..-2"
    "off x=-48..-32,y=-32..-16,z=-15..-5"
    "on x=-18..26,y=-33..15,z=-7..46"
    "off x=-40..-22,y=-38..-28,z=23..41"
    "on x=-16..35,y=-41..10,z=-47..6"
    "off x=-32..-23,y=11..30,z=-14..3"
    "on x=-49..-5,y=-3..45,z=-29..18"
    "off x=18..30,y=-20..-8,z=-3..13"
    "on x=-41..9,y=-7..43,z=-33..15"
    "on x=-54112..-39298,y=-85059..-49293,z=-27449..7877"
    "on x=967..23432,y=45373..81175,z=27513..53682"
    ))

(define long-regex
  #rx"(on|off) x=([-|1|2|3|4|5|6|7|8|9|0]+)..([-|1|2|3|4|5|6|7|8|9|0]+),y=([-|1|2|3|4|5|6|7|8|9|0]+)..([-|1|2|3|4|5|6|7|8|9|0]+),z=([-|1|2|3|4|5|6|7|8|9|0]+)..([-|1|2|3|4|5|6|7|8|9|0]+)")

(define HASH (make-hash))

(define (valid-xyz? x y z)
  (and (<= -50 x 50)
       (<= -50 y 50)
       (<= -50 z 50)))

(define (format-string str)
  (match-let ([(list _ on/off xi xf yi yf zi zf)
               (regexp-match long-regex str)])
    (match-let ([(list xi* xf* yi* yf* zi* zf*)
                 (map string->number (list xi xf yi yf zi zf))])
      (if (not (or (< xf* -50)
                   (> xi* 50)
                   (< yf* -50)
                   (> yi* 50)
                   (< zf* -50)
                   (> zi* 50)))
          (list on/off
                (for*/set ([x (range xi* (add1 xf*))]
                           [y (range yi* (add1 yf*))]
                           [z (range zi* (add1 zf*))])
                  (when (valid-xyz? x y z)
                    (string-join (map number->string (list x y z)) "_"))))
          (list "off" (set))))))

(define (compress-list los)
  (define (iter acc L)
    (cond
      [(empty? L) acc]
      [else
       (match-let ([(list on/off SET) (car L)])
         (if (string=? "on" on/off)
             (iter (set-union acc SET) (cdr L))
             (iter (set-subtract acc SET) (cdr L))))]))
  (iter (second (car los)) (cdr los)))

(define (entry-on? ent)
  (string=? "on" (cdr ent)))

(~> (pmapf format-string test)
    compress-list
    set->list
    (filter string? _)
    length)

(~> (pmapf format-string data)
    compress-list
    set->list
    (filter string? _)
    length)
