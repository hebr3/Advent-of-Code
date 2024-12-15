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

(define DP (make-hash))

(define (getcount line counts pos current_count countpos)
  (define key (list line counts pos current_count countpos))
  (cond
    [(hash-has-key? DP key)
     (hash-ref DP key)]
    [(= pos (string-length line))
     (if (= (length counts) countpos) 1 0)]
    [(char=? #\# (string-ref line pos))
     (getcount line counts (add1 pos) (add1 current_count) countpos)]
    [(or (char=? #\. (string-ref line pos))
         (= (length counts) countpos))
     (cond
       [(and (< countpos (length counts))
             (= current_count (list-ref counts countpos)))
        (getcount line counts (add1 pos) 0 (add1 countpos))]
       [(zero? current_count)
        (getcount line counts (add1 pos) 0 countpos)]
       [else 0])]
    [else
     (let ([hash_count (getcount line counts (add1 pos) (add1 current_count) countpos)]
           [dot_count 0])
       (cond
         [(= current_count (list-ref counts countpos))
          (set! dot_count (getcount line counts (add1 pos) 0 (add1 countpos)))]
         [(zero? current_count)
          (set! dot_count (getcount line counts (add1 pos) 0 countpos))])
       (hash-set! DP key (+ hash_count dot_count))
       (+ hash_count dot_count))]))      

(define (part-A L)
  (define rows (string-split L "\n"))
  (for/sum ([row rows])
    (match-let ([(list str nums) (string-split row)])
      (let ([line (string-join (list str ".") "")]
            [counts (map string->number (string-split nums ","))])
        (getcount line counts 0 0 0)))))

;;(part-A test)
;;(part-A data)

;;

(define (five-fold los)
  (for/list ([s los])
    (match-let ([(list str nums) (string-split s)])
      (string-join (list (string-join (make-list 5 str) "?")
                         (string-join (make-list 5 nums) ","))
                   " "))))

(define (part-B L)
  (define rows (string-split L "\n"))
  (for/sum ([row (five-fold rows)])
    (match-let ([(list str nums) (string-split row)])
      (let ([line (string-join (list str ".") "")]
            [counts (map string->number (string-split nums ","))])
        (getcount line counts 0 0 0)))))
  
;;(part-B test)
(part-B data)

(take (hash->list DP) 5)
(last (hash->list DP))

