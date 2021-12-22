#lang racket
(require threading)
(require racket/match)
(require lens)


(struct player [space score] #:transparent)
(struct game [player1 player2 roll-count] #:transparent)

(define deterministic-die
  (let ([n 0])
    (λ ([reset #f])
      (if reset
          (set! n 0)
          (begin0
            (+ (* 9 n) 6)
            (set! n (add1 n)))))))

(define data
  (~> "input/2021-20.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")))

(define test
  '("..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
    ""
    "#..#."
    "#...."
    "##..#"
    "..#.."
    "..###"))

(define (pad-image image)
  (define (pad-string str)
    (string-join (list (make-string 150 #\.) str (make-string 150 #\.)) ""))
  (let* ([len (string-length (car image))]
         [pad (make-string (+ 300 len) #\.)])
    (append (make-list 150 pad)
            (map pad-string image)
            (make-list 150 pad))))

(define (format-data data)
  (let ([key (car data)]
        [image (cddr data)])
    (list key (pad-image image))))

(define (list-of-char->number loc)
  (define (char->bin c) (if (char=? #\# c) #\1 #\0))
  (~> loc
      (map char->bin _)
      (append (list #\# #\b) _)
      (apply string _)
      string->number))

(define (image-mask-at-xy image x y)
  (if (and (< 0 x (sub1 (string-length (car image))))
           (< 0 y (sub1 (length image))))
      (flatten
       (for/list ([dy '(-1 0 1)])
         (for/list ([dx '(-1 0 1)])
           ;           (displayln (list (+ dx x) (+ dy y)))
           ;           (displayln (string-ref (list-ref image (+ dy y)) (+ dx x)))
           (string-ref (list-ref image (+ dy y)) (+ dx x)))))
      (list #\. #\. #\. #\. #\. #\. #\. #\. #\.)))

(define (update-image data)
  (match-let ([(list key image) data])
    (let ([Y (length image)]
          [X (string-length (car image))])
      (list key 
            (for/list ([y Y])
              (apply string
                     (map (λ (n) (string-ref key n))
                          (for/list ([x X])
                            (list-of-char->number (image-mask-at-xy image x y))))))))))

(define (strip-padding image)
  (define (strip-string str)
    (let ([width (string-length str)])
      (substring str 1 (sub1 width))))
  (let ([height (length image)]
        [width (string-length (car image))])
    (~> image
        (take _ (sub1 height))
        cdr
        (map strip-string _))))

(define (strip-padding2 data)
  (match-let ([(list key image) data])
    (list key (strip-padding image))))

(~> test
    format-data
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    update-image
    cadr
    (string-join _ "")
    string->list
    (count (λ (c) (char=? #\# c)) _)
    )

(~> data
    format-data
    
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    update-image
    update-image
    strip-padding2
    
    cadr
    strip-padding
    (string-join _ "")
    string->list
    (count (λ (c) (char=? #\# c)) _)
    )