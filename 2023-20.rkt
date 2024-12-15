#lang racket
(require data/queue)
(require (only-in "util.rkt" input->data tap point))

(define data (input->data "input/2023-19.txt"))

(define test1 "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")

(define test2 "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output")

;;

(define MODULES (make-hash))
(define PULSES (make-queue))

(define (->MODULE mod pulse)
  (match mod
    ["button"
     (->MODULE "broadcaster" pulse)
     (println (list 'button mod pulse))]
    ["boardcaster"
     (let ([destinations (hash-ref MODULES "broadcaster")])
       (for ([dest destinations])
         (->MODULE dest pulse)))
     (println (list 'broadcaster mod pulse))]
    [else
     (println (list 'else mod pulse))]))
        

(define (parse-row str)
  (match-let ([(list label ds) (string-split str " -> ")])
    (let ([destinations (string-split ds ", ")])
      (println (list label destinations))
      (cond
        [(string=? "broadcaster" label)
         (hash-set! MODULES label destinations)]
        [(string=? "%" (substring label 0 1))
         (hash-set! MODULES (substring label 1) destinations)] 
        [(string=? "&" (substring label 0 1))
         (hash-set! MODULES (substring label 1) destinations)]))))

(define (part-A L)
  (for ([row (string-split L "\n")])
    (displayln (parse-row row)))
  (->MODULE "button" 'low))

(part-A test1)
;(part-A test2)
;(part-A data)

MODULES

;;


(define (part-B L)
  L)

;(part-B test1)
;(part-B test2)
;(part-B data)











