#lang racket
(require threading)
(require racket/match)

;; Prep
(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)
      (string-split "\n")))

(define (test->list-of-strings str)
  (string-split str "\n"))

;; Structs

;; Functions
(define (find-problem L)
  (for/list ([i (- (length L) 5)])
    (let ([susp (take (drop L i) 5)]
          [targ (list-ref L (+ i 5))])
      (unless (for/or ([i susp])
                (member (- targ i) susp))
        targ))))

(define (find-problem2 L)
  (for/list ([i (- (length L) 25)])
    (let ([susp (take (drop L i) 25)]
          [targ (list-ref L (+ i 25))])
      (unless (for/or ([i susp])
                (member (- targ i) susp))
        targ))))

(define (eat-list target list)
  (define (iter sub-list list*)
    (cond
      [(= (apply + sub-list) target) sub-list]
      [(> (apply + sub-list) target) (eat-list target (rest list))]
      [else
       (iter (cons (first list*) sub-list) (rest list*))]))
  (iter '() list))

(define (max+min L)
  (+ (first (sort L <)) (last (sort L <))))

(define (find-problemB L)
  (define t (first (filter (λ (x) (not (void? x)))
                    (for/list ([i (- (length L) 5)])
                      (let ([susp (take (drop L i) 5)]
                            [targ (list-ref L (+ i 5))])
                        (unless (for/or ([i susp])
                                  (member (- targ i) susp))
                          targ))))))
  (max+min (eat-list t L)))

(define (find-problem2B L)
  (define t (first (filter (λ (x) (not (void? x)))
                    (for/list ([i (- (length L) 25)])
                      (let ([susp (take (drop L i) 25)]
                            [targ (list-ref L (+ i 25))])
                        (unless (for/or ([i susp])
                                  (member (- targ i) susp))
                          targ))))))
  (max+min (eat-list t L)))

;; Data
(define test
  (test->list-of-strings "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"))

(define data (file->list-of-strings "input/2020-09.txt"))


;; Puzzle
(display "test 1: ")
(~>> test
    (map string->number)
    find-problem
    (filter (λ (x) (not (void? x))))
    first)

(display "one: ")
(~>> data
    (map string->number)
    find-problem2
    (filter (λ (x) (not (void? x))))
    first)

(display "test 2: ")
(~>> test
    (map string->number)
    find-problemB)

(display "two: ")
(~>> data
    (map string->number)
    find-problem2B)
