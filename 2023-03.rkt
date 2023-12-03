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
  (define part-numbers (flatten (regexp-match* #px"\\d+" block)))
  (define row-length (index-of (string->list block) #\newline char=?))
  (for/sum ([pn part-numbers])
    (define pos (regexp-match-positions pn block))
    (match-let ([(cons (cons x y) _)pos])
      ;; top-right
      (define top-right (string-ref block (- x row-length 2)))
      ;; right
      (define right (string-ref block (sub1 x)))
      ;; bottom-right
      (define bottom-right (string-ref block (+ x row-length)))
      
      ;; top-left
      (define top-left (string-ref block (- y row-length 1)))
      ;; left
      (define left (string-ref block y))
      ;; bottom-left
      (define bottom-left (string-ref block (+ y row-length 1)))

      ;; top
      (define top (for/list ([i (in-range (- y x))])
                    (string-ref block (- y row-length i 2))))
      ;; bottom
      (define bottom (for/list ([i (in-range (- y x))])
                       (string-ref block (+ x row-length i 1))))

      (define neighbors (append (list top-right right bottom-right top-left left bottom-left) top bottom))
      (if (for/or ([n neighbors]) (not (char=? #\. n)))
          (string->number pn)
          0)
      )
    ))
  

(part-A test)
;(part-A data)

;;

(define (part-B L)
  L)

;(part-B test)
;(part-B data)

