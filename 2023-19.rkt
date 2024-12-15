#lang racket
(require data/queue)
(require (only-in "util.rkt" input->data tap point))

(define data (input->data "input/2023-19.txt"))

(define test "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}")

;;

(struct part [x m a s] #:transparent)

(define (parse->part str)
  (match-let ([(list _ x _ m _ a _ s) (string-split str #px"=|,|\\}")])
    (part (string->number x) (string->number m) (string->number a) (string->number s))))

(struct flow [name comparisons accept] #:transparent)

(define (parse->flow str FLOW-HASH)
  (let* ([inst (string-split str #px"\\{|:|,|\\}")]
         [name (first inst)]
         [accept (last inst)]
         [fl (flow name (take (drop inst 1) (- (length inst) 2)) accept)])
    (hash-set! FLOW-HASH name fl)))

(define (process part fl FLOW-HASH END-HASH)
  (match-let ([(flow name comparisons accept) (hash-ref FLOW-HASH fl)])
    (define (iter comp)
      (cond
        [(and (empty? comp) (string=? "A" accept))
         (hash-set! END-HASH "A" (cons part (hash-ref END-HASH "A" '())))]
        [(and (empty? comp) (string=? "R" accept))
         (hash-set! END-HASH "R" (cons part (hash-ref END-HASH "R" '())))]
        [(empty? comp)
         (process part accept FLOW-HASH END-HASH)]
        [else
         (let* ([equation (first comp)]
                [ps (string-split equation #px">|<")]
                [greater? (string-contains? equation ">")]
                [label (first ps)]
                [num (string->number (second ps))]
                [loc (second comp)])
           (println (list equation greater? label num loc))
           (cond
             ;; x
             [(and (string=? "x" label) greater?)
              (if (< num (part-x part))
                  (if (string=? "A" loc)
                      (hash-set! END-HASH "A" (cons part (hash-ref END-HASH "A" '())))
                      (if (string=? "R" loc)
                          (hash-set! END-HASH "R" (cons part (hash-ref END-HASH "R" '())))
                          (process part loc FLOW-HASH END-HASH)))
                  (iter (drop comp 2)))]
             [(string=? "x" label)
              (if (> num (part-x part))
                  (if (string=? "A" loc)
                      (hash-set! END-HASH "A" (cons part (hash-ref END-HASH "A" '())))
                      (if (string=? "R" loc)
                          (hash-set! END-HASH "R" (cons part (hash-ref END-HASH "R" '())))
                          (process part loc FLOW-HASH END-HASH)))
                  (iter (drop comp 2)))]

             ;; m
             [(and (string=? "m" label) greater?)
              (if (< num (part-m part))
                  (if (string=? "A" loc)
                      (hash-set! END-HASH "A" (cons part (hash-ref END-HASH "A" '())))
                      (if (string=? "R" loc)
                          (hash-set! END-HASH "R" (cons part (hash-ref END-HASH "R" '())))
                          (process part loc FLOW-HASH END-HASH)))
                  (iter (drop comp 2)))]
             [(string=? "m" label)
              (if (> num (part-m part))
                  (if (string=? "A" loc)
                      (hash-set! END-HASH "A" (cons part (hash-ref END-HASH "A" '())))
                      (if (string=? "R" loc)
                          (hash-set! END-HASH "R" (cons part (hash-ref END-HASH "R" '())))
                          (process part loc FLOW-HASH END-HASH)))
                  (iter (drop comp 2)))]

             ;; a
             [(and (string=? "a" label) greater?)
              (if (< num (part-a part))
                  (if (string=? "A" loc)
                      (hash-set! END-HASH "A" (cons part (hash-ref END-HASH "A" '())))
                      (if (string=? "R" loc)
                          (hash-set! END-HASH "R" (cons part (hash-ref END-HASH "R" '())))
                          (process part loc FLOW-HASH END-HASH)))
                  (iter (drop comp 2)))]
             [(string=? "a" label)
              (if (> num (part-a part))
                  (if (string=? "A" loc)
                      (hash-set! END-HASH "A" (cons part (hash-ref END-HASH "A" '())))
                      (if (string=? "R" loc)
                          (hash-set! END-HASH "R" (cons part (hash-ref END-HASH "R" '())))
                          (process part loc FLOW-HASH END-HASH)))
                  (iter (drop comp 2)))]

             ;; s
             [(and (string=? "s" label) greater?)
              (if (< num (part-s part))
                  (if (string=? "A" loc)
                      (hash-set! END-HASH "A" (cons part (hash-ref END-HASH "A" '())))
                      (if (string=? "R" loc)
                          (hash-set! END-HASH "R" (cons part (hash-ref END-HASH "R" '())))
                          (process part loc FLOW-HASH END-HASH)))
                  (iter (drop comp 2)))]
             [(string=? "s" label)
              (if (> num (part-s part))
                  (if (string=? "A" loc)
                      (hash-set! END-HASH "A" (cons part (hash-ref END-HASH "A" '())))
                      (if (string=? "R" loc)
                          (hash-set! END-HASH "R" (cons part (hash-ref END-HASH "R" '())))
                          (process part loc FLOW-HASH END-HASH)))
                  (iter (drop comp 2)))]))]))
    (iter comparisons)))

(define (part-A L)
  (let* ([blocks (string-split L "\n\n")]
         [flows (string-split (first blocks) "\n")]
         [FLOW-HASH (make-hash)]
         [parts (string-split (second blocks) "\n")]
         [END-HASH (make-hash)])
    (for ([f flows])
      (parse->flow f FLOW-HASH))
    (for ([p parts])
      (process (parse->part p) "in" FLOW-HASH END-HASH))
    (for/sum ([p (hash-ref END-HASH "A")])
      (match-let ([(part x m a s) p])
        (+ x m a s)))))

;(part-A test)
;(part-A data)

;;


(define (new-ranges var op n xl xh ml mh al ah sl sh)
  (match (list var op)
    [(list "x" ">") (list (max xl (add1 n)) xh ml mh al ah sl sh)]
    [(list "x" "<") (list xl (min xh (sub1 n)) ml mh al ah sl sh)]
    [(list "m" ">") (list xl xh (max ml (add1 n)) mh al ah sl sh)]
    [(list "m" "<") (list xl xh ml (min mh (sub1 n)) al ah sl sh)]
    [(list "a" ">") (list xl xh ml mh (max al (add1 n)) ah sl sh)]
    [(list "a" "<") (list xl xh ml mh al (min ah (sub1 n)) sl sh)]
    [(list "s" ">") (list xl xh ml mh al ah (max sl (add1 n)) sh)]
    [(list "s" "<") (list xl xh ml mh al ah sl (min sh (sub1 n)))]))



(define (part-B L)
  (let* ([top-block (first (string-split L "\n\n"))]
         [flows (string-split top-block "\n")]
         [FLOWS-HASH (make-hash)]
         [Q (make-queue)])
    (for ([f flows])
      (match-let ([(list name comps) (string-split f #px"\\{|\\}")])
        (hash-set! FLOWS-HASH name comps)))
    

    (enqueue! Q (list "in" 1 4000 1 4000 1 4000 1 4000))
    (define (iter acc)
      (cond
        [(queue-empty? Q) acc]
        [else
         (match-let ([(list state xl xh ml mh al ah sl sh) (dequeue! Q)])
           (println (list state xl xh ml mh al ah sl sh))
           (cond
             [(or (> xl xh) (> ml mh) (> al ah) (> sl sh)) (iter acc)]
             [(string=? "A" state)
              (iter (+ acc
                       (* (- xh (add1 xl))
                          (- mh (add1 ml))
                          (- ah (add1 al))
                          (- sh (add1 sl)))))]
             [(string=? "R" state)
              (iter acc)]
             [else
              (let ([rule (hash-ref FLOWS-HASH state)])
                (for ([cmd (string-split rule ",")])
                  (let ([applies #t]
                        [res (string-copy cmd)])
                    (cond
                      [(string-contains? cmd ":")
                       (match-let ([(list C R) (string-split cmd ":")])
                         (let ([var (substring C 0 1)]
                               [op (substring C 1 2)]
                               [n (string->number (substring C 3))])
                           (enqueue! Q (cons R (new-ranges var op n xl xh ml mh al ah sl sh)))
                           (println (cons R (new-ranges var op n xl xh ml mh al ah sl sh))))
                         (match-let ([(list state xl xh ml mh al ah sl sh)
                                      (cons R (new-ranges (substring C 0 1) (substring C 1 2) (string->number (substring C 3)) xl xh ml mh al ah sl sh))])
                           (println (list state xl xh ml mh al ah sl sh))))
                       (iter acc)]
                      [else
                       (enqueue! Q (list res xl xh ml mh al ah sl sh))
                       ;(iter acc)
                       ]))))]))]))   
    (iter 0)
    ))                      


(part-B test)
;(part-B data)











