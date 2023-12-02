#lang racket
(require threading)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2016-06.txt"))

(define test "eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar")

;;

(define (group-positions LoLoC)
  (for/list ([i (length (car LoLoC))])
    (for/list ([j LoLoC])
      (list-ref j i))))

(define (part-A L)
  (~> L
      (string-split "\n")
      (map string->list _)
      group-positions
      (map (λ (col) (group-by (λ (x) x) col)) _)
      (map (λ (col) (sort col (λ (x y) (> (length x) (length y))))) _)
      (map first _)
      (map first _)
      (apply string _)
      ))

(part-A test)
(part-A data)

;;

(define (part-B L)
  (~>> L
       (string-split _ "\n")
      (map string->list _)
      group-positions
      (map (λ (col) (group-by (λ (x) x) col)) _)
      (map (λ (col) (sort col (λ (x y) (< (length x) (length y))))) _)
      (map first _)
      (map first _)
      (apply string _)
       ))

(part-B test)
(part-B data)
