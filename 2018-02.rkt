#lang racket
(require threading)

(define test1
  (list "abcdef"
        "bababc"
        "abbcde"
        "abcccd"
        "aabcdd"
        "abcdee"
        "ababab"))

(define test2
  (list "abcde"
        "fghij"
        "klmno"
        "pqrst"
        "fguij"
        "axcye"
        "wvxyz"))

(define data
  (~> "input.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")))

(define (not-one? x)
  (not (= 1 x)))

(define (has-two? list-of-numbers)
  (member 2 list-of-numbers))

(define (has-three? list-of-numbers)
  (member 3 list-of-numbers))

(define (find-groups str)
  (~> str
      string->list
      (sort _ char<?)
      (group-by identity _)
      (map length _)
      (filter not-one? _)
      remove-duplicates))

(define (close-match string1 string2)
  (define (shared-char s1 s2)
    (apply string (for/list ([i s1][j s2]) (if (char=? i j) i #\space))))
  (define (remove-spaces str)
    (string-replace str " " ""))
  (let ([matches (shared-char string1 string2)])
    (~> matches
        remove-spaces
        string-length
        (= _ (sub1 (string-length string1)))
        (if _
            (remove-spaces matches)
            #f))))

(define (find-close-match list-of-strings)
  (or (for/or ([i (rest list-of-strings)])
        (close-match (first list-of-strings) i))
      (find-close-match (rest list-of-strings))))

(display "test: ")
(displayln
 (~> test1
     (map find-groups _)
     ((λ (x) (* (count has-three? x) (count has-two? x))) _)))

(display "test2: ")
(displayln
 (~> test2
     find-close-match))

(display "one: ")
(displayln
 (~> data
     (map find-groups _)
     ((λ (x) (* (count has-three? x) (count has-two? x))) _)))

(display "two: ")
(displayln
 (~> data
     find-close-match))

