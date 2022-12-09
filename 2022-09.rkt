#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2022-09.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")
      ))

(define test '("R 4"
               "U 4"
               "L 3"
               "D 1"
               "R 4"
               "D 1"
               "L 5"
               "R 2"))
(define test2 '("R 5"
                "U 8"
                "L 8"
                "D 3"
                "R 17"
                "D 10"
                "L 25"
                "U 20"))
;; helper

(struct point [x y] #:transparent)

(struct input [dir dist] #:transparent)
(define (parse-input str)
  (match-let ([(list dr ds) (string-split str " ")])
    (input dr (string->number ds))))

(define (right h t d acc)
  (match-let ([(point hx hy) h][(point tx ty) t])
    (cond
      [(zero? d) (list h t acc)]
      [(<= hx tx) (right (point (add1 hx) hy) t (sub1 d) (cons t acc))]
      [else (right (point (add1 hx) hy) h (sub1 d) (cons h acc))])))

(define (left h t d acc)
  (match-let ([(point hx hy) h][(point tx ty) t])
    (cond
      [(zero? d) (list h t acc)]
      [(>= hx tx) (left (point (sub1 hx) hy) t (sub1 d) (cons t acc))]
      [else (left (point (sub1 hx) hy) h (sub1 d) (cons h acc))])))

(define (up h t d acc)
  (match-let ([(point hx hy) h][(point tx ty) t])
    (cond
      [(zero? d) (list h t acc)]
      [(<= hy ty) (up (point hx (add1 hy)) t (sub1 d) (cons t acc))]
      [else (up (point hx (add1 hy)) h (sub1 d) (cons h acc))])))

(define (down h t d acc)
  (match-let ([(point hx hy) h][(point tx ty) t])
    (cond
      [(zero? d) (list h t acc)]
      [(>= hy ty) (down (point hx (sub1 hy)) t (sub1 d) (cons t acc))]
      [else (down (point hx (sub1 hy)) h (sub1 d) (cons h acc))])))

(define (read-inputs LoI)
  (define (iter acc H T L*)
    (if (empty? L*)
        acc
        (match-let ([(input DR DS) (first L*)])
          (cond
            [(string=? "R" DR)
             (match-let ([(list newH newT newAcc) (right H T DS '())])
               (iter (cons newAcc acc) newH newT (rest L*)))]
            [(string=? "L" DR)
             (match-let ([(list newH newT newAcc) (left H T DS '())])
               (iter (cons newAcc acc) newH newT (rest L*)))]
            [(string=? "U" DR)
             (match-let ([(list newH newT newAcc) (up H T DS '())])
               (iter (cons newAcc acc) newH newT (rest L*)))]
            [(string=? "D" DR)
             (match-let ([(list newH newT newAcc) (down H T DS '())])
               (iter (cons newAcc acc) newH newT (rest L*)))]
            [else 'missing]))))
  (iter '() (point 0 0) (point 0 0) LoI))

;;---
  
(define (part-A L)
  (~>> L
       (map parse-input)
       read-inputs
       flatten
       list->set
       set->list
       length))

(part-A test)
(part-A data)

;;---

(define (follow p1 p2)
  (match-let ([(point p1x p1y) p1][(point p2x p2y) p2])
    (cond
      [(and (<= (abs (- p1x p2x)) 1) (<= (abs (- p1y p2y)) 1))
       p2]
      [(and (= p1x p2x) (< p1y p2y))
       (point p2x (sub1 p2y))]
      [(and (< p1x p2x) (< p1y p2y))
       (point (sub1 p2x) (sub1 p2y))]
      [(and (< p1x p2x) (= p1y p2y))
       (point (sub1 p2x) p2y)]
      [(and (< p1x p2x) (> p1y p2y))
       (point (sub1 p2x) (add1 p2y))]

      [(and (= p1x p2x) (> p1y p2y))
       (point p2x (add1 p2y))]
      [(and (> p1x p2x) (> p1y p2y))
       (point (add1 p2x) (add1 p2y))]
      [(and (> p1x p2x) (= p1y p2y))
       (point (add1 p2x) p2y)]
      [(and (> p1x p2x) (< p1y p2y))
       (point (add1 p2x) (sub1 p2y))]
      )))

(define (right2 rope dist acc)
  (if (zero? dist)
      (list rope acc)
      (match-let ([(list h1 h2 h3 h4 h5 h6 h7 h8 h9 t) rope])
        (let* ([h1* (point (add1 (point-x h1)) (point-y h1))]
               [h2* (follow h1* h2)]
               [h3* (follow h2* h3)]
               [h4* (follow h3* h4)]
               [h5* (follow h4* h5)]
               [h6* (follow h5* h6)]
               [h7* (follow h6* h7)]
               [h8* (follow h7* h8)]
               [h9* (follow h8* h9)]
               [t* (follow h9* t)])
          (right2 (list h1* h2* h3* h4* h5* h6* h7* h8* h9* t*) (sub1 dist) (cons t* acc))))))
(define (left2 rope dist acc)
  (if (zero? dist)
      (list rope acc)
      (match-let ([(list h1 h2 h3 h4 h5 h6 h7 h8 h9 t) rope])
        (let* ([h1* (point (sub1 (point-x h1)) (point-y h1))]
               [h2* (follow h1* h2)]
               [h3* (follow h2* h3)]
               [h4* (follow h3* h4)]
               [h5* (follow h4* h5)]
               [h6* (follow h5* h6)]
               [h7* (follow h6* h7)]
               [h8* (follow h7* h8)]
               [h9* (follow h8* h9)]
               [t* (follow h9* t)])
          (left2 (list h1* h2* h3* h4* h5* h6* h7* h8* h9* t*) (sub1 dist) (cons t* acc))))))    
(define (up2 rope dist acc)
  (if (zero? dist)
      (list rope acc)
      (match-let ([(list h1 h2 h3 h4 h5 h6 h7 h8 h9 t) rope])
        (let* ([h1* (point (point-x h1) (add1 (point-y h1)))]
               [h2* (follow h1* h2)]
               [h3* (follow h2* h3)]
               [h4* (follow h3* h4)]
               [h5* (follow h4* h5)]
               [h6* (follow h5* h6)]
               [h7* (follow h6* h7)]
               [h8* (follow h7* h8)]
               [h9* (follow h8* h9)]
               [t* (follow h9* t)])
          (up2 (list h1* h2* h3* h4* h5* h6* h7* h8* h9* t*) (sub1 dist) (cons t* acc))))))
(define (down2 rope dist acc)
  (if (zero? dist)
      (list rope acc)
      (match-let ([(list h1 h2 h3 h4 h5 h6 h7 h8 h9 t) rope])
        (let* ([h1* (point (point-x h1) (sub1 (point-y h1)))]
               [h2* (follow h1* h2)]
               [h3* (follow h2* h3)]
               [h4* (follow h3* h4)]
               [h5* (follow h4* h5)]
               [h6* (follow h5* h6)]
               [h7* (follow h6* h7)]
               [h8* (follow h7* h8)]
               [h9* (follow h8* h9)]
               [t* (follow h9* t)])
          (down2 (list h1* h2* h3* h4* h5* h6* h7* h8* h9* t*) (sub1 dist) (cons t* acc))))))

(define (read-inputs2 LoI)
  (define o (point 0 0))
  (define (iter acc rope L*)
    (if (empty? L*)
        acc
        (match-let ([(input DIR DIST) (first L*)])
          (cond
            [(string=? "R" DIR)
             (match-let ([(list rope* acc*) (right2 rope DIST '())])
               (iter (cons acc* acc) rope* (rest L*)))]
            [(string=? "L" DIR)
             (match-let ([(list rope* acc*) (left2 rope DIST '())])
               (iter (cons acc* acc) rope* (rest L*)))]
            [(string=? "U" DIR)
             (match-let ([(list rope* acc*) (up2 rope DIST '())])
               (iter (cons acc* acc) rope* (rest L*)))]
            [(string=? "D" DIR)
             (match-let ([(list rope* acc*) (down2 rope DIST '())])
               (iter (cons acc* acc) rope* (rest L*)))]))))
  (iter '() (list o o o o o o o o o o) LoI))

(define (part-B L)
  (~>> L
       (map parse-input)
       read-inputs2
       flatten
       list->set
       set->list
       length))

(part-B test2)
(part-B data)