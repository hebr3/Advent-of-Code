#lang racket
(require data/queue)

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

;; Test data
(define data (input->data "input/2024-21.txt"))

(define test "379A
")

;; Structs
;; (define keypad-str "789
;; 456
;; 123
;;  0A")

;; (define arrows-str " ^A
;; <v>")

;; Helper
(define (lookup pt)
  (match pt
    [(list #\A #\0) "<A"]
    [(list #\A #\1) "^<<A"]
    [(list #\A #\3) "^A"]
    [(list #\A #\4) "^^<<A"]
    [(list #\A #\7) "^^^<<A"]
    [(list #\A #\8) "^^^<A"]
    [(list #\A #\9) "^^^A"]
    [(list #\0 #\2) "^A"]
    [(list #\0 #\A) ">A"]
    [(list #\1 #\7) "^^A"]
    [(list #\1 #\3) ">>A"]
    [(list #\2 #\9) "^^>A"]
    [(list #\3 #\A) "vA"]
    [(list #\3 #\7) "^^<<A"]
    [(list #\3 #\6) "^A"]
    [(list #\4 #\1) "vA"]
    [(list #\4 #\5) ">A"]
    [(list #\5 #\6) ">A"]
    [(list #\5 #\A) "vv>A"]
    [(list #\6 #\A) "vvA"]
    [(list #\7 #\3) ">>vvA"]
    [(list #\7 #\8) ">A"]
    [(list #\7 #\9) ">>A"]
    [(list #\8 #\0) "vvvA"]
    [(list #\8 #\3) ">vvvA"]
    [(list #\8 #\5) "vA"]
    [(list #\9 #\7) "<<A"]
    [(list #\9 #\8) "<A"]
    [(list #\9 #\A) "vvvA"]
    
    [(list #\< #\A) ">>^A"]
    [(list #\v #\A) ">^A"]
    [(list #\> #\A) "^A"]
    [(list #\^ #\A) ">A"]
    [(list #\A #\A) "A"]
 
    [(list #\< #\^) ">^A"]
    [(list #\v #\^) "^A"]
    [(list #\> #\^) "<^A"]
    [(list #\^ #\^) "A"]
    [(list #\A #\^) "<A"]
 
    [(list #\< #\>) ">>A"]
    [(list #\v #\>) ">A"]
    [(list #\> #\>) "A"]
    [(list #\^ #\>) "v>A"]
    [(list #\A #\>) "vA"]
 
    [(list #\< #\v) ">A"]
    [(list #\v #\v) "A"]
    [(list #\> #\v) "<A"]
    [(list #\^ #\v) "vA"]
    [(list #\A #\v) "<vA"]
 
    [(list #\< #\<) "A"]
    [(list #\v #\<) "<A"]
    [(list #\> #\<) "<<A"]
    [(list #\^ #\<) "<vA"]
    [(list #\A #\<) "<<vA"]
    ))

(define (build-instructions str)
  (define loc (cons #\A (string->list str)))
  (apply string-append
         (for/list ([c1 loc][c2 (rest loc)])
           (lookup (list c1 c2)))))

(build-instructions (build-instructions (build-instructions "vA<A")))
(build-instructions (build-instructions (build-instructions ">A^A")))

(build-instructions (build-instructions (build-instructions ">AvA")))
(build-instructions (build-instructions (build-instructions "^A<A")))

;; (define (press-direction key-str)
;;   (define (N c) (match c [#\v #\^][#\> #\A]))
;;   (define (S c) (match c [#\^ #\v][#\A #\>]))
;;   (define (E c) (match c [#\^ #\A][#\< #\v][#\v #\>]))
;;   (define (W c) (match c [#\A #\^][#\v #\<][#\> #\v]))
;;   
;;   (define keys (string->list key-str))
;;   (define (iter position L acc)
;;     (displayln (list L acc))
;;     (cond
;;       [(empty? L) (apply string (reverse acc))]
;;       [(char=? #\A (first L))
;;        (iter position (rest L) (cons position acc))]
;;       [else
;;        (match (first L)
;;          [#\^ (iter (N position) (rest L) acc)]
;;          [#\v (iter (S position) (rest L) acc)]
;;          [#\> (iter (E position) (rest L) acc)]
;;          [#\< (iter (W position) (rest L) acc)])]))
;;   (iter #\A keys '()))
;; 
;; (define (press-keypad key-str)
;;   (define (N c) (match c [#\0 #\2][#\A #\3][#\1 #\4][#\2 #\5][#\3 #\6][#\4 #\7][#\5 #\8][#\6 #\9]))
;;   (define (S c) (match c [#\2 #\0][#\3 #\A][#\4 #\1][#\5 #\2][#\6 #\3][#\7 #\4][#\8 #\5][#\9 #\6]))
;;   (define (E c) (match c [#\7 #\8][#\8 #\9][#\4 #\5][#\5 #\6][#\1 #\2][#\2 #\3][#\0 #\A]))
;;   (define (W c) (match c [#\8 #\7][#\9 #\8][#\5 #\4][#\6 #\5][#\2 #\1][#\3 #\2][#\A #\0]))
;; 
;;   (define keys (string->list key-str))
;;   (define (iter position L acc)
;;     (displayln (list L acc))
;;     (cond
;;       [(empty? L) (apply string (reverse acc))]
;;       [(char=? #\A (first L))
;;        (iter position (rest L) (cons position acc))]
;;       [else
;;        (match (first L)
;;          [#\^ (iter (N position) (rest L) acc)]
;;          [#\v (iter (S position) (rest L) acc)]
;;          [#\> (iter (E position) (rest L) acc)]
;;          [#\< (iter (W position) (rest L) acc)])]))
;;   (iter #\A keys '()))
;; 
;; (define (undo-depth-3 str)
;;   (press-keypad (press-direction (press-direction str))))
;; 
(define (depth-3 inst)
  (build-instructions (build-instructions (build-instructions inst))))
 
;; (define (complexity-score str intr)
;;   (list (string->number (substring str 0 3))
;;         (string-length intr)
;;         (depth-3 str)
;;         (undo-depth-3 (depth-3 str))))
;; 
;; ;; Main Function
;; 
;; (define (part-A input)
;;   (define lines (string-split input "\n"))
;; 
;;   (for/list ([line lines])
;;     ;(displayln line)
;;     ;(displayln (complexity-score line (depth-3 line)))
;;     ;(displayln (undo-depth-3 (depth-3 line)))
;;     ;(displayln (build-instructions line))
;;     ;(displayln (build-instructions (build-instructions line)))
;;     ;(displayln (depth-3 line))
;;     (complexity-score line (depth-3 line)))
;;   )
;; 
;; (part-A test)
;; ;(part-A data)
;;   
;; ;;
;; 
;; (define (part-B input)
;;   input)
;; 
;; ;(part-B test)
;; ;(part-B data)
 
(for ([c1 "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"]
      [c2 (depth-3 "379A")]
      [i (in-range 17)])
  (displayln (list c1 c2)))

(string-length (depth-3 "379A"))