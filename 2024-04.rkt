#lang racket
(require threading)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2024-04.txt"))

(define test0 "..X...
.SAMX.
.A..A.
XMAS.S
.X....")

(define test1 "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(define (horizontal-xmas los)
  (for/sum ([str los])
    (length (flatten (list (regexp-match* #px"XMAS" str)
                           (regexp-match* #px"SAMX" str))))))

(define (list-of-strings-ref los r c)
  (string-ref (list-ref los r) c))

(define (rotate-los los)
  (for/list ([i (string-length (first los))])
    (apply string (for/list ([j (length los)])
                    (list-of-strings-ref los j i)))))

(define (vertical-xmas los)
  (horizontal-xmas (rotate-los los)))

(define (skew-los-left los)
  (for/list ([i (length los)])
    (string-append (make-string i #\.)
                   (list-ref los i)
                   (make-string (- (length los) i) #\.))))

(define (positive-xmas los)
  (vertical-xmas (skew-los-left los)))

(define (skew-los-right los)
  (for/list ([i (length los)])
    (string-append (make-string (- (length los) i) #\.)
                   (list-ref los i)
                   (make-string i #\.))))

(define (negative-xmas los)
  (vertical-xmas (skew-los-right los)))

(define (part-A L)
  (let* ([LINES (string-split L "\n")])
    (+ (horizontal-xmas LINES)
       (vertical-xmas LINES)
       (positive-xmas LINES)
       (negative-xmas LINES))))

(part-A test0)
(part-A test1)
(part-A data)

;;

(define test2 ".M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........")

;; GOOGLE overlapping matches in Racket
(define (find-overlapping-matches pattern str)
  (let loop ([pos 0]
             [matches '()])
    (define match (regexp-match-positions pattern str pos))
    (if match
        (let ([start (caar match)]
              [end (cdar match)])
          (loop (add1 start) 
                (cons (substring str start end) matches)))
        (reverse matches))))

(define (make-mask top bot len)
  (let ([padding (make-string (- len 2) #\.)])
    (pregexp (string-append top padding "A" padding bot))))

(define (length-nested-lists . lists-of-lists)
  (length (flatten lists-of-lists)))

(define (part-B L)
  (let* ([LINES (string-split L "\n")]
         [STR-LENGTH (string-length (first LINES))]
         [MASK-1 (make-mask "M.M" "S.S" STR-LENGTH)]
         [MASK-2 (make-mask "M.S" "M.S" STR-LENGTH)]
         [MASK-3 (make-mask "S.S" "M.M" STR-LENGTH)]
         [MASK-4 (make-mask "S.M" "S.M" STR-LENGTH)])
  (for/sum ([i (in-range (- (length LINES) 2))])
    (let ([SUBSTRING (apply string-append (take (drop LINES i) 3))])
      (length-nested-lists (find-overlapping-matches MASK-1 SUBSTRING)
                           (find-overlapping-matches MASK-2 SUBSTRING)
                           (find-overlapping-matches MASK-3 SUBSTRING)
                           (find-overlapping-matches MASK-4 SUBSTRING))))))

(part-B test2)
(part-B data)
