#lang racket
(require threading)
(require racket/match)
(require "util.rkt")

(define data (input->data "input/2023-07.txt"))

(define test "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

;;

;; (struct hand [cards grp bid] #:transparent)
;;
;; (define (card>? a b)
;;   (define card-order (string->list "AKQJT98765432"))
;;   (define (iter A B)
;;     (if (char=? (first A) (first B))
;;         (iter (rest A) (rest B))
;;         (< (index-of card-order (first A) char=?)
;;            (index-of card-order (first B) char=?))))
;;   (iter (string->list a) (string->list b)))
;;
;; (define (list>? a b)
;;   (> (length a) (length b)))
;;
;; (define (string->grp str)
;;   (~>> str
;;        string->list
;;        (group-by id)
;;        (sort _ list>?)))
;;
;; (define (string->hand str)
;;   (match-let ([(list c b) (string-split str)])
;;     (hand c (string->grp c) (string->number b))))
;;
;; (define (hand>? a b)
;;   (match-let* ([(hand ca ga ba) a]
;;                [ga-count (map length ga)]
;;                [(hand cb gb bb) b]
;;                [gb-count (map length gb)])
;;     (cond
;;       [(> (first ga-count) (first gb-count)) #t]
;;       [(< (first ga-count) (first gb-count)) #f]
;;       [(and (= (first ga-count) (first gb-count))
;;             (> (second ga-count) (second gb-count))) #t]
;;       [(and (= (first ga-count) (first gb-count))
;;             (< (second ga-count) (second gb-count))) #f]
;;       [else (card>? ca cb)])))
;;
;; (define (part-A L)
;;   (define hands (map string->hand (string-split L "\n")))
;;   (for/sum ([h (reverse (sort hands hand>?))]
;;              [n (in-naturals)])
;;     (* (hand-bid h) (add1 n))))
;;
;; (part-A test)
;; (part-A data)

(struct hand [card grp bid] #:transparent)


(define (extend g n)
  (cons (+ n (first g)) (rest g)))

(define (string->grp str)
  (let* ([str* (string-replace str "J" "")]
         [grp (group-by id (string->list str*))])
    (cond
      [(string=? "JJJJJ" str) '(5)]
      [(string=? str str*) (sort (map length grp) >)]
      [else (extend (sort (map length grp) >) (- 5 (string-length str*)))])))

(define (string->hand str)
  (match-let ([(list c b) (string-split str)])
    (hand c (string->grp c) (string->number b))))

(define (cards>? a b)
   (define card-order (string->list "AKQT98765432J"))
   (define (iter A B)
     (if (char=? (first A) (first B))
         (iter (rest A) (rest B))
         (< (index-of card-order (first A) char=?)
            (index-of card-order (first B) char=?))))
   (iter (string->list a) (string->list b)))

(define (hand>? A B)
  (match-let ([(hand ca ga ba) A]
              [(hand cb gb bb) B])
    (cond
      [(= 5 (first ga) (first gb)) (cards>? ca cb)]
      [(> (first ga) (first gb)) #t]
      [(< (first ga) (first gb)) #f]
      [(> (second ga) (second gb)) #t]
      [(< (second ga) (second gb)) #f]
      [else (cards>? ca cb)])))

(define (part-B L)
  (define hands (map string->hand (string-split L "\n")))
  (for/sum ([h (reverse (sort hands hand>?))]
            [n (in-naturals)])
    (* (hand-bid h) (add1 n))))

(part-B test)
(part-B data)
