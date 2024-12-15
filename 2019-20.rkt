#lang racket
(require rackunit)
(require threading)
(require racket/match)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2019-20.txt"))

(define test1 "         A           
         A           
  #######.#########  
  #######.........#  
  #######.#######.#  
  #######.#######.#  
  #######.#######.#  
  #####  B    ###.#  
BC...##  C    ###.#  
  ##.##       ###.#  
  ##...DE  F  ###.#  
  #####    G  ###.#  
  #########.#####.#  
DE..#######...###.#  
  #.#########.###.#  
FG..#########.....#  
  ###########.#####  
             Z       
             Z       
")

(define test2 "                   A               
                   A               
  #################.#############  
  #.#...#...................#.#.#  
  #.#.#.###.###.###.#########.#.#  
  #.#.#.......#...#.....#.#.#...#  
  #.#########.###.#####.#.#.###.#  
  #.............#.#.....#.......#  
  ###.###########.###.#####.#.#.#  
  #.....#        A   C    #.#.#.#  
  #######        S   P    #####.#  
  #.#...#                 #......VT
  #.#.#.#                 #.#####  
  #...#.#               YN....#.#  
  #.###.#                 #####.#  
DI....#.#                 #.....#  
  #####.#                 #.###.#  
ZZ......#               QG....#..AS
  ###.###                 #######  
JO..#.#.#                 #.....#  
  #.#.#.#                 ###.#.#  
  #...#..DI             BU....#..LF
  #####.#                 #.#####  
YN......#               VT..#....QG
  #.###.#                 #.###.#  
  #.#...#                 #.....#  
  ###.###    J L     J    #.#.###  
  #.....#    O F     P    #.#...#  
  #.###.#####.#.#####.#####.###.#  
  #...#.#.#...#.....#.....#.#...#  
  #.#####.###.###.#.#.#########.#  
  #...#.#.....#...#.#.#.#.....#.#  
  #.###.#####.###.###.#.#.#######  
  #.#.........#...#.............#  
  #########.###.###.#############  
           B   J   C               
           U   P   P               
")

;;

(struct point [x y] #:transparent)
(struct portal-point [name x y] #:transparent)

(define (los-ref* los x y)
  (let ([y-length (length los)]
        [x-length (string-length (first los))])
    (if (or (< x 0) (<= x-length x) (< y 0) (<= y-length y))
        #\space
        (string-ref (list-ref los y) x))))

(define (path? los x y)
  (char=? #\. (los-ref* los x y)))

(define (map-char? los x y)
  (let ([ch (los-ref* los x y)])
    (if (string-contains? "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (string ch))
        ch
        #f)))

(define (portal? los x y)
  (and (map-char? los x y)
       (or (path? los (add1 x) y)
           (path? los (sub1 x) y)
           (path? los x (add1 y))
           (path? los x (sub1 y)))))

(define (xy->portal los x y)
  (let ([first-char (los-ref* los x y)]
        [second-char (or (map-char? los (add1 x) y)
                      (map-char? los (sub1 x) y)
                      (map-char? los x (add1 y))
                      (map-char? los x (sub1 y)))])
  (portal-point (string first-char second-char) x y)))

(define (string-reverse str)
  (apply string (reverse (string->list str))))

(define (hash-ref-neighbor adj-hash pt)
  (match-let ([(point x y) pt])
    (let ([up (point x (sub1 y))]
          [dw (point x (add1 y))]
          [lt (point (sub1 x) y)]
          [rt (point (add1 x) y)])
      (cond
        [(hash-has-key? adj-hash up) up]
        [(hash-has-key? adj-hash dw) dw]
        [(hash-has-key? adj-hash lt) lt]
        [(hash-has-key? adj-hash rt) rt]))))

(define (hash-update! adj-hash pt-1 pt-2)
  (hash-set! adj-hash pt-1 (cons pt-2 (hash-ref adj-hash pt-1))))

(define (dijkstra adj-hash start)
  (define DIST-HASH (make-hash))
  (for ([node (hash-keys adj-hash)])
    (hash-set! DIST-HASH node +inf.0))
  (hash-set! DIST-HASH start 0)
  (define (dij* lop d)
    (cond
      [(empty? lop) DIST-HASH]
      [else
       (define new-lop '())
       (for ([p lop])
         (hash-set! DIST-HASH p d)
         (set! new-lop (flatten (cons (hash-ref adj-hash p) new-lop))))
       (set! new-lop (remove-duplicates (filter (λ (x) (infinite? (hash-ref DIST-HASH x))) new-lop)))
       (dij* new-lop (add1 d))]))
  (dij* (list start) 0))

;  DIST-HASH)

(define (part-A L)
  (define rows (string-split L "\n"))
  
  (define PASSAGES '())
  (define PORTALS '())
  (for* ([y (length rows)]
         [x (string-length (first rows))])
    (when (path? rows x y)
      (set! PASSAGES (cons (point x y) PASSAGES)))
    (when (portal? rows x y)
      (set! PORTALS (cons (xy->portal rows x y) PORTALS))))

  (for ([pl PORTALS])
    (match-let ([(portal-point name x y) pl])
      (cond
        [(string=? "AA" name)
         (set! PASSAGES (cons (point x y) PASSAGES))]
        [(string=? "ZZ" name)
         (set! PASSAGES (cons (point x y) PASSAGES))])))

  (define ADJ-HASH (make-hash))
  (for ([pt PASSAGES])
    (hash-set! ADJ-HASH pt '())
    (match-let ([(point x y) pt])
      (let ([up (point x (sub1 y))]
            [dw (point x (add1 y))]
            [lt (point (sub1 x) y)]
            [rt (point (add1 x) y)])
        (when (member up PASSAGES)
          (hash-update! ADJ-HASH pt up))
        (when (member dw PASSAGES)
          (hash-update! ADJ-HASH pt dw))
        (when (member lt PASSAGES)
          (hash-update! ADJ-HASH pt lt))
        (when (member rt PASSAGES)
          (hash-update! ADJ-HASH pt rt)))))

  (for ([i (in-range 0 (- (length PORTALS) 2))])
    (for ([j (in-range (add1 i) (- (length PORTALS) 1))])
      (match-let ([(portal-point ni xi yi) (list-ref PORTALS i)]
                  [(portal-point nj xj yj) (list-ref PORTALS j)])
        (when (or (string=? ni nj) (string=? ni (string-reverse nj)))
          (let* ([point-i (point xi yi)]
                 [adj-i (hash-ref-neighbor ADJ-HASH point-i)]
                 [point-j (point xj yj)]
                 [adj-j (hash-ref-neighbor ADJ-HASH point-j)])
            (hash-update! ADJ-HASH adj-i adj-j)
            (hash-update! ADJ-HASH adj-j adj-i))))))

  (for ([p PORTALS]) (println p))
  ;ADJ-HASH
  ;(BFS (point 9 1) (point 13 17) ADJ-HASH)
;  (println (hash->list (dijkstra ADJ-HASH (point 9 1))))
  (define DIST (hash->list (dijkstra ADJ-HASH (point 37 1))))
  (for ([p DIST])
    (match-let ([(cons (point x y) n) p])
      (string-set! (list-ref rows y) x (if (infinite? n) #\? (first (string->list (number->string (modulo n 10))))))))
  (for ([row rows])
    (println row))
  DIST
  )
  

;(part-A test1)
;(part-A test2)
(part-A data)