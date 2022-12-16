#lang racket
(require threading)

(struct Point [x y] #:transparent)

(define (Point->String p)
  (format "(~a,~a)" (Point-x p) (Point-y p)))

(define (m-dist p1 p2)
  (match-let ([(Point x1 y1) p1][(Point x2 y2) p2])
    (+ (abs (- y1 y2))
       (abs (- x1 x2)))))

(struct Sensor [x y d] #:transparent)

(define (Sensor->String s)
  (match-let ([(Sensor x y d) s])
    (format "(~a,~a,~a)" x y d)))

(define (Point->Sensor s b)
  (match-let ([(Point x y) s])
    (Sensor x y (m-dist s b))))

(define test2
  (list
   (Sensor 2 18 7)
   (Sensor 9 16 1)
   (Sensor 13 2 3)
   (Sensor 12 14 4)
   (Sensor 10 20 4)
   (Sensor 14 17 5)
   (Sensor 8 7 9)
   (Sensor 2 0 10)
   (Sensor 0 11 3)
   (Sensor 20 14 8)
   (Sensor 17 20 6)
   (Sensor 16 7 5)
   (Sensor 14 3 1)
   (Sensor 20 1 7)))

(define data2
  (list
   (Sensor 2391367 3787759 612816)
   (Sensor 1826659 2843839 521776)
   (Sensor 980874 2369046 1318562)
   (Sensor 2916267 2516612 557389)
   (Sensor 3304786 844925 1502817)
   (Sensor 45969 76553 1938058)
   (Sensor 2647492 1985479 301445)
   (Sensor 15629 2015720 31449)
   (Sensor 3793239 3203486 422557)
   (Sensor 3998240 15268 1931751)
   (Sensor 3475687 3738894 430403)
   (Sensor 3993022 3910207 1012683)
   (Sensor 258318 2150378 377338)
   (Sensor 1615638 1108834 1882770)
   (Sensor 1183930 3997648 1274762)
   (Sensor 404933 3377916 1434027)
   (Sensor 3829801 2534117 1128488)
   (Sensor 2360813 2494240 493995)
   (Sensor 2286195 3134541 690610)
   (Sensor 15626 1984269 31463)
   (Sensor 3009341 3849969 1007824)
   (Sensor 1926292 193430 1116775)
   (Sensor 3028318 3091480 770748)))

;;;;;

(define (u-> n)
  (format "~a+x" n))
(define (d-> n)
  (format "~a-x" n))

(define (Sensor->list s)
  (match-let ([(Sensor x y d) s])
    (let ([x-min (- x d 1)]
          [x-max (+ x d 1)])
      (let ([b1 (- y x-min)] ;/
            [b2 (+ y x-min)] ;\
            [b3 (- y x-max)] ;/
            [b4 (+ y x-max)]);\
        (list b1 b2 b3 b4)))))

(define (l->ups l)
  (match-let ([(list b1 _ b3 _) l])
    (list b1 b3)))
(define (l->downs l)
  (match-let ([(list _ b2 _ b4) l])
    (list b2 b4)))

(define (valid-boarder s N)
  (match-let ([(Sensor x y d) s])
    (define x-min (if (< 0 (- x d)) (- x d) 0))
    (define y-min (if (< 0 (- y d)) (- y d) 0))
    (define x-max (if (< (+ x d 1) N) (+ x d 1) N))
    (define y-max (if (< (+ y d 1) N) (+ y d 1) N))
    (displayln (Sensor->String s))
    (displayln (list x-min x-max y-min y-max))
    (for/list ([x* (in-range x-min x-max)])
      (for/list ([y* (in-range y-min y-max)])
        (Point x* y*)))))

(define (part-B inputs N)
  (define (not-inside? s b p)
    (and (not (equal? s p))
         (not (equal? b p))
         (> (m-dist s p) (m-dist s b))))
  (define freq 4000000)
  (define L (map Sensor->list inputs))
  (define ups (map l->ups L))
  (define downs (map l->downs L))
  (define possible-solves
    (flatten
     (for/list ([i (map first (take (sort (group-by (λ (x) x) (flatten ups)) (λ (a b) (> (length a) (length b)))) 3))])
       (for/list ([j (map first (take (sort (group-by (λ (x) x) (flatten downs)) (λ (a b) (> (length a) (length b)))) 3))])
         (Point (/ (- j i) 2) (+ (/ (- j i) 2) i))))))
  (for/or ([p possible-solves])
    (for/and ([i inputs])
      (match-let ([(Point px py) p][(Sensor sx sy sd) i])
        (displayln (list (Point->String p) (Sensor->String i) (< sd (m-dist p (Point sx sy)))))
        (and (<= 0 px N)
             (<= 0 py N)
             (< sd (m-dist p (Point sx sy)))
             (+ py (* freq px)))))))

(part-B test2 20)
(part-B data2 4000000)

