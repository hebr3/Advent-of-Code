#lang racket
(require threading)
(require racket/match)

(define (input->data filename)
  (~> filename
      (open-input-file #:mode 'text)
      (read-line 'return-linefeed)
      ))

(define data (input->data "input/2016-04.txt"))

(define test "aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]")

(define (strip-dashes str)
  (string-replace str #px"\\d|-" ""))

(define (code->hash str)
  (define H (make-hash))
  (for ([c str])
    (let ([C (string c)])
      (if (hash-has-key? H C)
          (hash-set! H C (+ 1 (hash-ref H C)))
          (hash-set! H C 1))))
  (hash->list H))

(define (code>? list-one list-two)
  (if (= (cdr list-one) (cdr list-two))
      (string<? (car list-one) (car list-two))
      (> (cdr list-one) (cdr list-two))))

(define (get-id str)
  (let ([SL (string-length str)])
    (string->number (substring str (- SL 10) (- SL 7)))))

(define (string->door str)
  (match-let ([(list code check) (string-split str #px"\\[|\\]")])
    (~> code
        strip-dashes
        code->hash
        (sort _ code>?)
        (map car _)
        (take _ 5)
        (string-join _ "")
        (string=? _ check)
        (if _ (get-id str) 0)
        )))

(define (part-A L)
  (~>> L
      (string-split _ "\n")
      (map string->door)
      (apply + _)
      ;(count (λ (x) x))
      ))

(part-A test)
(part-A data)

;;

(define (shift-cipher str num)
  (apply string
         (for/list ([ch str])
            (if (char=? ch #\-)
                #\space
                (~> ch
                    char->integer
                    (- _ 97)
                    (+ _ num)
                    (modulo _ 26)
                    (+ _ 97)
                    integer->char)))))

(define (code->rotate str)
  (let ([code (substring str 0 (- (string-length str) 4))]
        [num (string->number (substring str (- (string-length str) 3)))])
    (list (shift-cipher code num) num)))

(define (string->door2 str)
  (match-let ([(list code check) (string-split str #px"\\[|\\]")])
    (~> code
        strip-dashes
        code->hash
        (sort _ code>?)
        (map car _)
        (take _ 5)
        (string-join _ "")
        (string=? _ check)
        (if _ (code->rotate code) (list "" 0))
        )))

(define (part-B L)
  (~>> L
       (string-split _ "\n")
       (map string->door2)
       (filter (λ (x) (string-contains? (car x) "north")) _)
       ))

(part-B data)
