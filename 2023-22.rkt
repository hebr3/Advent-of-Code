#lang racket
(require data/queue)
(require (only-in "util.rkt" input->data tap point))

(define data (input->data "input/2023-22.txt"))

(define test "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9")

;;

(struct block [x y z] #:transparent) 

(define (block<? a b)
  (< (block-z (first a)) (block-z (first b))))

(define (str->block str)
  (match-let ([(list x y z dx dy dz)
               (map string->number (string-split str #px",|~"))])
    (cond
      [(< 1 (count (λ (x) x) (list (not (= x dx)) (not (= y dy)) (not (= z dz)))))
       (displayln (list 'not-a-bar str x y z dx dy dz))]
      [(not (= x dx)) (for/list ([i (in-range x (add1 dx))]) (block i y z))]
      [(not (= y dy)) (for/list ([i (in-range y (add1 dy))]) (block x i z))]
      [(not (= z dz)) (for/list ([i (in-range z (add1 dz))]) (block x y i))]
      [else (list (block x y z))])))

(define (can-drop? lob tower)
  ;(println (list 'can-drop? lob))
  (let ([tower* (for/list ([b tower] #:when (not (equal? lob b))) b)])
    (for/and ([b lob])
      (match-let ([(block x y z) b])
        (and (< 1 z)
             (not (member (block x y (sub1 z)) (flatten tower*))))))))

(define (drop-block lob i)
  ;(println (list 'drop-block lob))
  (for/list ([b lob])
    (match-let ([(block x y z) b])
      (block x y (- z i)))))
      

(define (update-tower tower i)
  ;(println (list 'update-tower i))
  (define (iter t* acc)
    (cond
      [(empty? t*) acc]
      [(can-drop? (first t*) tower)
       (iter (cons (drop-block (first t*) 1) (rest t*)) acc)]
      [else
       (iter (rest t*) (cons (first t*) acc))]))
  (let ([new-tower (reverse (iter tower '()))])
    (if (equal? tower new-tower)
        tower
        (update-tower new-tower (add1 i)))))

(define (take* lst num)
  (if (< num (length lst))
      (take lst num)
      lst))

(define (part-A L)
  (let* ([input-rows (string-split L "\n")]
         [init-blocks (take* (sort (map str->block input-rows) block<?) 200)]
         [init-tower (update-tower init-blocks 0)])
    (for/sum ([lob init-tower])
      (let* ([pos-tower (for/list ([b init-tower] #:when (not (equal? b lob))) b)]
             [next-tower (update-tower pos-tower 0)])
        ;(println (list 'lob lob))
        (if (equal? pos-tower next-tower)
            1
            0))
      )))


(part-A test)
;(part-A data)

;;

(define (part-B L)
  L)

;(part-B test)
;(part-B data)











