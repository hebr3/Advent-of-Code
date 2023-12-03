#lang racket
(require threading)
(require file/md5)
(require racket/match)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2023-03.txt"))

(define test "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

;;

(define (pad-block str)
  (define rows (string-split str "\n"))
  (define row-len (string-length (first rows)))
  (define blank-row (make-string (+ row-len 2) #\.))
  (~>> rows
       (map (λ (s) (string-join (list "." s ".") "")) _)
       (append (list blank-row) _ (list blank-row))
       (string-join _ "\n")))

(define (part-A L)
  (define block (pad-block L))
  ;(for ([s (string-split block "\n")])
  ;  (displayln s))
  (define row-length (index-of (string->list block) #\newline char=?))
  (define (iter idx acc)
    (define c1 (string-ref block idx))
    (define c2 (string-ref block (+ 1 idx)))
    (define c3 (string-ref block (+ 2 idx)))
    (define hundreds (list (- idx row-length 2) (- idx row-length 1) (- idx row-length) (- idx row-length -1) (- idx row-length -2)
                           (- idx 1) (+ idx 3)
                           (+ idx row-length) (+ idx row-length 1) (+ idx row-length 2) (+ idx row-length 3) (+ idx row-length 4)))
    (define tens (list (- idx row-length 2) (- idx row-length 1) (- idx row-length) (- idx row-length -1)
                       (- idx 1) (+ idx 2)
                       (+ idx row-length) (+ idx row-length 1) (+ idx row-length 2) (+ idx row-length 3)))
    (define ones (list (- idx row-length 2) (- idx row-length 1) (- idx row-length)
                       (- idx 1) (+ idx 1)
                       (+ idx row-length) (+ idx row-length 1) (+ idx row-length 2)))
    (define (valid? c)
      (and (not (member c (list #\newline #\.)))
           (member c (string->list "1234567890"))))
    (define (symbol? c)
      (not (char=? #\. c)))
    ;(println (list c1 c2 c3))
    (cond
      [(< (- (string-length block) row-length) idx)
       acc]
      [(and (valid? c1) (valid? c2) (valid? c3))
       (define pn (string->number (string c1 c2 c3)))
       (if (for/or ([i hundreds]) (symbol? (string-ref block i)))
           (iter (+ 3 idx) (+ acc pn))
           (iter (+ 3 idx) acc))]
      [(and (valid? c1) (valid? c2))
       (define pn (string->number (string c1 c2)))
       (if (for/or ([i tens]) (symbol? (string-ref block i)))
           (iter (+ 2 idx) (+ acc pn))
           (iter (+ 2 idx) acc))]
      [(valid? c1)
       (define pn (string->number (string c1)))
       (if (for/or ([i ones]) (symbol? (string-ref block i)))
           (iter (+ 1 idx) (+ acc pn))
           (iter (+ 1 idx) acc))]
      [else
       (iter (add1 idx) acc)]))
  (iter (add1 row-length) 0))
  

(part-A test)
(part-A data)

;;

(define (part-B L)
  (define block (pad-block L))
  (define gear-locs (map car (regexp-match-positions* #px"\\*" block)))
  (define number-locs (regexp-match-positions* #px"\\d+" block))
  (define row-length (index-of (string->list block) #\newline char=?))
  (define (get-neighbors idx)
    (for/list ([n number-locs])
      (match-let ([(cons x y) n])
        (or (and (member (+ idx 1) (range x y)) ; right
                 (string->number (substring block x y)))
            
            (and (member (- idx row-length) (range x y)) ; right-top
                 (string->number (substring block x y)))

            (and (member (- idx row-length 1) (range x y)) ; top
                 (string->number (substring block x y)))

            (and (member (- idx row-length 2) (range x y)) ; left-top
                 (string->number (substring block x y)))

            (and (member (- idx 1) (range x y)) ; left
                 (string->number (substring block x y)))

            (and (member (+ idx row-length) (range x y)) ; left-bottom
                 (string->number (substring block x y)))

            (and (member (+ idx row-length 1) (range x y)) ; bottom
                 (string->number (substring block x y)))

            (and (member (+ idx row-length 2) (range x y)) ; right-bottom
                 (string->number (substring block x y)))

            )
        )))
  (for/sum ([g gear-locs])
    (let ([gears (filter (λ (x) x) (get-neighbors g))])
      (if (= 2 (length gears))
          (* (first gears) (second gears))
          0))))

(part-B test)
(part-B data)


;; (define test2 ".....
;; .12..
;; ..*..
;; .789.
;; .....")
;; 
;; (part-B test2)
