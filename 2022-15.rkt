#lang typed/racket

;;;;;

;(: data String)
;(define data "Sensor at x=2391367, y=3787759: closest beacon is at x=2345659, y=4354867
;Sensor at x=1826659, y=2843839: closest beacon is at x=1654342, y=3193298
;Sensor at x=980874, y=2369046: closest beacon is at x=31358, y=2000000
;Sensor at x=2916267, y=2516612: closest beacon is at x=3064453, y=2107409
;Sensor at x=3304786, y=844925: closest beacon is at x=3064453, y=2107409
;Sensor at x=45969, y=76553: closest beacon is at x=31358, y=2000000
;Sensor at x=2647492, y=1985479: closest beacon is at x=2483905, y=2123337
;Sensor at x=15629, y=2015720: closest beacon is at x=31358, y=2000000
;Sensor at x=3793239, y=3203486: closest beacon is at x=3528871, y=3361675
;Sensor at x=3998240, y=15268: closest beacon is at x=4731853, y=1213406
;Sensor at x=3475687, y=3738894: closest beacon is at x=3528871, y=3361675
;Sensor at x=3993022, y=3910207: closest beacon is at x=3528871, y=3361675
;Sensor at x=258318, y=2150378: closest beacon is at x=31358, y=2000000
;Sensor at x=1615638, y=1108834: closest beacon is at x=2483905, y=2123337
;Sensor at x=1183930, y=3997648: closest beacon is at x=1654342, y=3193298
;Sensor at x=404933, y=3377916: closest beacon is at x=1654342, y=3193298
;Sensor at x=3829801, y=2534117: closest beacon is at x=3528871, y=3361675
;Sensor at x=2360813, y=2494240: closest beacon is at x=2483905, y=2123337
;Sensor at x=2286195, y=3134541: closest beacon is at x=1654342, y=3193298
;Sensor at x=15626, y=1984269: closest beacon is at x=31358, y=2000000
;Sensor at x=3009341, y=3849969: closest beacon is at x=3528871, y=3361675
;Sensor at x=1926292, y=193430: closest beacon is at x=1884716, y=-881769
;Sensor at x=3028318, y=3091480: closest beacon is at x=3528871, y=3361675")

;(: test String)
;(define test "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
;Sensor at x=9, y=16: closest beacon is at x=10, y=16
;Sensor at x=13, y=2: closest beacon is at x=15, y=3
;Sensor at x=12, y=14: closest beacon is at x=10, y=16
;Sensor at x=10, y=20: closest beacon is at x=10, y=16
;Sensor at x=14, y=17: closest beacon is at x=10, y=16
;Sensor at x=8, y=7: closest beacon is at x=2, y=10
;Sensor at x=2, y=0: closest beacon is at x=2, y=10
;Sensor at x=0, y=11: closest beacon is at x=2, y=10
;Sensor at x=20, y=14: closest beacon is at x=25, y=17
;Sensor at x=17, y=20: closest beacon is at x=21, y=22
;Sensor at x=16, y=7: closest beacon is at x=15, y=3
;Sensor at x=14, y=3: closest beacon is at x=15, y=3
;Sensor at x=20, y=1: closest beacon is at x=15, y=3")

;;;;;

(struct Point ([x : Real][y : Real]) #:transparent)

(: data (Listof (List Point Point)))
(define data
  (list
   (list (Point 2391367 3787759) (Point 2345659 4354867))
   (list (Point 1826659 2843839) (Point 1654342 3193298))
   (list (Point 980874 2369046) (Point 31358 2000000))
   (list (Point 2916267 2516612) (Point 3064453 2107409))
   (list (Point 3304786 844925) (Point 3064453 2107409))
   (list (Point 45969 76553) (Point 31358 2000000))
   (list (Point 2647492 1985479) (Point 2483905 2123337))
   (list (Point 15629 2015720) (Point 31358 2000000))
   (list (Point 3793239 3203486) (Point 3528871 3361675))
   (list (Point 3998240 15268) (Point 4731853 1213406))
   (list (Point 3475687 3738894) (Point 3528871 3361675))
   (list (Point 3993022 3910207) (Point 3528871 3361675))
   (list (Point 258318 2150378) (Point 31358 2000000))
   (list (Point 1615638 1108834) (Point 2483905 2123337))
   (list (Point 1183930 3997648) (Point 1654342 3193298))
   (list (Point 404933 3377916) (Point 1654342 3193298))
   (list (Point 3829801 2534117) (Point 3528871 3361675))
   (list (Point 2360813 2494240) (Point 2483905 2123337))
   (list (Point 2286195 3134541) (Point 1654342 3193298))
   (list (Point 15626 1984269) (Point 31358 2000000))
   (list (Point 3009341 3849969) (Point 3528871 3361675))
   (list (Point 1926292 193430) (Point 1884716 -881769))
   (list (Point 3028318 3091480) (Point 3528871 3361675))))

(: test (Listof (List Point Point)))
(define test
  (list (list (Point 2 18) (Point -2 15)) (list (Point 9 16) (Point 10 16)) (list (Point 13 2) (Point 15 3)) (list (Point 12 14) (Point 10 16)) (list (Point 10 20) (Point 10 16)) (list (Point 14 17) (Point 10 16)) (list (Point 8 7) (Point 2 10)) (list (Point 2 0) (Point 2 10)) (list (Point 0 11) (Point 2 10)) (list (Point 20 14) (Point 25 17)) (list (Point 17 20) (Point 21 22)) (list (Point 16 7) (Point 15 3)) (list (Point 14 3) (Point 15 3)) (list (Point 20 1) (Point 15 3))))

(: String->Real (-> String Real))
(define (String->Real str)
  (define num (string->number str))
  (if (number? num)
      (real-part num)
      0))

(: Point->String (-> Point String))
(define (Point->String p)
  (format "(~a,~a)" (Point-x p) (Point-y p)))

(: parse-line (-> String (List Point Point)))
(define (parse-line str)
  (let* ([parts (string-split str #px"=|,|:|\n")]
         [x1 (String->Real (list-ref parts 1))]
         [y1 (String->Real (list-ref parts 3))]
         [x2 (String->Real (list-ref parts 5))]
         [y2 (String->Real (list-ref parts 7))])
    (list (Point x1 y1) (Point x2 y2))))

(: dist (-> Real Real Real Real Real))
(define (dist x1 x2 y1 y2)
  (+ (if (< x2 x1) (- x1 x2) (- x2 x1))
     (abs (- y1 y2))))

(: m-dist (-> Point Point Real))
(define (m-dist p1 p2)
  (let ([x1 (Point-x p1)]
        [y1 (Point-y p1)]
        [x2 (Point-x p2)]
        [y2 (Point-y p2)])
    (dist x1 x2 y1 y2)))

;;;;;

(: part-A (-> (Listof (List Point Point)) Real Real))
(define (part-A inputs N)
  ;(: inputs (Listof (List Point Point)))
  ;(define inputs (map parse-line (string-split L "\n")))
  (: inside? (-> Point Point Point Boolean))
  (define (inside? s b p)
    (let ([sx (Point-x s)]
          [sy (Point-y s)]
          [bx (Point-x b)]
          [by (Point-y b)]
          [px (Point-x p)]
          [py (Point-y p)])
      (and (not (and (= sx px) (= sy py)))
           (not (and (= bx px) (= by py)))
           (<= (dist sx px sy py) (dist sx bx sy by)))))
  (count (λ (x) x)
         (for/list : (Listof Boolean) ([x (in-range 10000000)])
           (for/or : Boolean ([i inputs])
             (inside? (first i) (second i) (Point (- x 5000000) N))))))

;(part-A test 10)
;(part-A data 2000000)

;;;;;

(: part-B (-> (Listof (List Point Point)) Integer Integer))
(define (part-B inputs N)
  ;(: inputs (Listof (List Point Point)))
  ;(define inputs (map parse-line (string-split L "\n")))
  (: not-inside? (-> Point Point Point Boolean))
  (define (not-inside? s b p)
    (let ([sx (Point-x s)]
          [sy (Point-y s)]
          [bx (Point-x b)]
          [by (Point-y b)]
          [px (Point-x p)]
          [py (Point-y p)])
      (and (not (and (= sx px) (= sy py)))
           (not (and (= bx px) (= by py)))
           (> (dist sx px sy py) (dist sx bx sy by)))))
  (define freq 4000000)
  (for/sum : Integer ([x (in-range (add1 N))])
    (for/sum : Integer ([y (in-range (add1 N))])
      (if (for/and : Boolean ([i inputs])
            (not-inside? (first i) (second i) (Point x y)))
          (+ y (* freq x))
          0))))

;(part-B test 20)
;(part-B data 4000000)

