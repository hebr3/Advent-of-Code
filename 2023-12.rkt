#lang racket
(require racket/match)
(require "util.rkt")

(define data (input->data "input/2023-12.txt"))

(define demo "#.#.### 1,1,3
.#...#....###. 1,1,3
.#.###.#.###### 1,3,1,6
####.#...#... 4,1,1
#....######..#####. 1,6,5
.###.##....# 3,2,1")

(define test "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
")

;;

(define (find-?-marks str)
  (for/list ([c str][i (in-naturals)] #:when (char=? #\? c))
    i))

(struct record [springs data question-marks spring-count] #:transparent)

(define (string->record str)
  (match-let ([(list s d) (string-split str " ")])
    (let ([d* (map string->number (string-split d ","))]
          [qm (find-?-marks s)]
          [sc (for/sum ([c s] #:when (char=? #\# c)) 1)])
      (record s d* qm sc))))

(define (record-good? rec)
  (match-let ([(record test-str real-lengths qm sc) rec])
    (let* ([test-springs (string-split test-str #px"\\.+")]
           [test-lengths (map string-length test-springs)])
    (equal? test-lengths real-lengths))))

;; (map (compose record-good? string->record) (string-split demo "\n"))

(define (build-test str comb)
  (apply string
         (for/list ([c str][i (in-naturals)])
           (cond
             [(member i comb) #\#]
             [(char=? #\? c) #\.]
             [else c]))))

(define (test-record rec)
  (match-let ([(record test-str real-lengths qm sc) rec])
    (let* ([delta (- (apply + real-lengths) sc)]
           [combs (combinations qm delta)])
      (for/sum ([comb combs])
        (if (record-good? (record (build-test test-str comb) real-lengths qm sc))
            1 0)))))
        

(define (part-A L)
  (define records (map string->record (string-split L "\n")))
  (for/sum ([rec records])
    (println rec)
    (test-record rec)))

;; (part-A test)
;; (part-A data)

;;

(define (five-fold-springs str)
  (string-join (make-list 5 str) "?"))
(define (five-fold-data str)
  (string-join (make-list 5 str) ","))

(define (string->record* str)
  (match-let ([(list s d) (string-split str " ")])
    (let* ([s* (five-fold-springs s)]
           [d* (map string->number (string-split (five-fold-data d) ","))]
           [qm (find-?-marks s*)]
           [sc (for/sum ([c s*] #:when (char=? #\# c)) 1)])
      (record s* d* qm sc))))

(define (part-B L)
  (define records (map string->record* (string-split L "\n")))
  (for/sum ([rec records])
    (test-record rec)))
  
 
(part-B test)
;; (part-B data)
