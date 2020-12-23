#lang racket
(require threading)
(require racket/match)
(require rackunit)

;; Prep
(define (file->list-of-strings filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)
      (string-split "\n")))

(define (test->list-of-strings str)
  (~> str
      (string-split "\n")))

;; Struct
(struct recipe [ingredients allergens] #:transparent)

;; Functions
(define (string->recipe str)
  (match-let ([(list _ ing alg) (regexp-match #rx"(.+) \\(contains (.+)\\)" str)])
    (recipe (apply set (string-split ing)) (apply set (string-split alg ", ")))))

(define (->list-of-recipes los)
  (map string->recipe los))

(define (get-allergens list-of-recipes)
  (~>> list-of-recipes
       (map recipe-allergens)
       (apply set-union)))

(define (intersect-of-allergens list-of-recipes)
  (apply set-union
  (for/list ([allergen (get-allergens list-of-recipes)])
    (define (recipe-contains-allergen? recipe)
      (set-member? (recipe-allergens recipe) allergen))
    (~>> list-of-recipes
         (filter recipe-contains-allergen?)
         (map recipe-ingredients)
         (apply set-intersect)))))

(define (get-all-ingredients list-of-recipes)
  (~>> list-of-recipes
       (map recipe-ingredients)
       (apply set-union)))

(define (find-non-allergens list-of-recipes)
  (let ([ingredients (get-all-ingredients list-of-recipes)]
        [allergens (intersect-of-allergens list-of-recipes)])
    (set-subtract ingredients allergens)))

(define (count-ingredients ingredients list-of-recipes)
  (for/sum ([ing ingredients])
    (define (contains-ingredient? recipe)
      (set-member? (recipe-ingredients recipe) ing))
    (~>> list-of-recipes
         (filter contains-ingredient?)
         length)))

(define (intersect-of-allergens2 list-of-recipes)
  (for/list ([allergen (get-allergens list-of-recipes)])
    (define (recipe-contains-allergen? recipe)
      (set-member? (recipe-allergens recipe) allergen))
    (~>> list-of-recipes
         (filter recipe-contains-allergen?)
         (map recipe-ingredients)
         (apply set-intersect)
         (cons allergen))))

(define (match-allergen-to-ingredient assoc-list-of-allergen/ingredients)
  (define (one-choice? x)
    (= 1 (set-count (cdr x))))
  (define (iter acc lst)
    (match-let ([(list (cons alg S))
                 (filter one-choice? lst)])
      (define (not-allergen? x)
        (not (string=? (car x) alg)))
      (filter one-choice? lst)))
  (iter '() assoc-list-of-allergen/ingredients))

;; Data
(define data
  (~>> "input/2020-21.txt"
       file->list-of-strings))

(define test
  (~>> "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)
"
       test->list-of-strings))


;; Puzzle
(display "test 1: ")
(~>> test
     ->list-of-recipes
     find-non-allergens
     (count-ingredients _ (->list-of-recipes test)))

(display "one: ")
(~>> data
     ->list-of-recipes
     find-non-allergens
     (count-ingredients _ (->list-of-recipes data)))

(display "test 2: ")
(~>> test
     ->list-of-recipes
     intersect-of-allergens2)

(display "two: ")
(~>> data
     ->list-of-recipes
     intersect-of-allergens2
     match-allergen-to-ingredient)

;; Unit Test
