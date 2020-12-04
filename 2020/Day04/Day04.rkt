#lang racket
(require threading)
(require racket/match)

(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)))

(define (valid? str)
  (for/and ([i '("ecl:" "pid:" "eyr:" "hcl:" "byr:" "iyr:" "hgt:")])
    (string-contains? str i)))

(define (valid2? str)
  (and (valid? str)
       (<= 1920 (string->number (first (regexp-match* #rx"byr:(....)" str #:match-select cadr))) 2002)
       (<= 2010 (string->number (first (regexp-match* #rx"iyr:(....)" str #:match-select cadr))) 2020)
       (<= 2020 (string->number (first (regexp-match* #rx"eyr:(....)" str #:match-select cadr))) 2030)
       (hcl? str)
       (hgt? str)
       (ecl? str)
       (pid? str)))

(define (hcl? str)
  (and (string-contains? str "hcl:#")
       (= 6 (string-length (first (regexp-match* #rx"hcl:\\#([0-9a-z]+)" str #:match-select cadr))))))

(define (hgt? str)
  (let ([cm (regexp-match* #rx"hgt:(...)cm" str #:match-select cadr)]
        [in (regexp-match* #rx"hgt:(..)in" str #:match-select cadr)])
    (cond
      [(string-contains? str "cm") (and (not (empty? cm))
                                        (<= 150 (string->number (first cm)) 193))]
      [(string-contains? str "in") (and (not (empty? in))
                                        (<= 59 (string->number (first in)) 76))]
      [else #f])))

(define (ecl? str)
  (~> str
      (string-split _ #px" |\n")
      (filter (λ (x) (string-contains? x "ecl")) _)
      first
      (string-split _ ":")
      second
      (set-member? (set "amb" "blu" "brn" "gry" "grn" "hzl" "oth") _)))

(define (pid? str)
  (define (help x)
    (if (not (empty? x))
        (= 9 (string-length (first x)))
        #f))
  (~> str
      (regexp-match* #rx"pid:([0-9]+)" _ #:match-select cadr)
      help))



(define test
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(define test2
  "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")

(define test3
  "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

(define data (file->list-of-strings "input.txt"))

(display "test 1: ")
(~> test
    (string-split "\n\n")
    (map valid? _)
    (count identity _))

(display "one: ")
(~> data
    (string-split "\n\n")
    (map valid? _)
    (count identity _))

(display "test 2: ")
(~> test2
    (string-split "\n\n")
    (map valid2? _)
    (count identity _))

(display "test 3: ")
(~> test3
    (string-split "\n\n")
    (map valid2? _)
    (count identity _))

(display "two: ")
(~> data
    (string-split "\n\n")
    (map valid2? _)
    (count identity _))