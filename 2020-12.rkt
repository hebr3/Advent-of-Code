#lang racket
(require threading)
(require racket/match)
(require rackunit)

;; Prep
(define (file->vector-of-char filename)
  (~> filename
      open-input-file
      (read-line 'return-linefeed)
      (string-split "\n")))

(define (test->vector-of-char str)
  (~> str
      (string-split "\n")))

;; Struct
(struct ferry [facing X Y] #:transparent)
(struct instruction [act val] #:transparent)

;; Functions
(define (string->instruction str)
  (instruction (string->symbol (substring str 0 1)) (string->number (substring str 1))))

(define (turn-clockwise ferry*)
  (match-let ([(ferry facing X Y) ferry*])
    (cond
      [(eq? facing 'N) (ferry 'E X Y)]
      [(eq? facing 'E) (ferry 'S X Y)]
      [(eq? facing 'S) (ferry 'W X Y)]
      [(eq? facing 'W) (ferry 'N X Y)])))

(define (forward-ferry ferry* val)
  (match-let ([(ferry facing X Y) ferry*])
    (cond
      [(eq? 'N facing) (ferry 'N X (+ Y val))]
      [(eq? 'S facing) (ferry 'S X (- Y val))]
      [(eq? 'E facing) (ferry 'E (+ X val) Y)]
      [(eq? 'W facing) (ferry 'W (- X val) Y)])))

(define (turn-ferry ferry* dir deg)
  (if (eq? dir 'R)
      (cond
        [(= 90 deg) (turn-clockwise ferry*)]
        [(= 180 deg) (turn-clockwise (turn-clockwise ferry*))]
        [(= 270 deg) (turn-clockwise (turn-clockwise (turn-clockwise ferry*)))]
        [else ferry*])
      (cond
        [(= 90 deg) (turn-clockwise (turn-clockwise (turn-clockwise ferry*)))]
        [(= 180 deg) (turn-clockwise (turn-clockwise ferry*))]
        [(= 270 deg) (turn-clockwise ferry*)]
        [else ferry*])))

(define (move-ferry ferry* act val)
  (match-let ([(ferry facing X Y) ferry*])
    (cond
      [(eq? 'N act) (ferry facing X (+ Y val))]
      [(eq? 'S act) (ferry facing X (- Y val))]
      [(eq? 'E act) (ferry facing (+ X val) Y)]
      [(eq? 'W act) (ferry facing (- X val) Y)])))

(define (update-ferry instr ferry*)
  (match-let ([(ferry face x y) ferry*]
              [(instruction act val) instr])
    (cond
      [(or (eq? 'R act) (eq? 'L act)) (turn-ferry ferry* act val)]
      [(eq? 'F act) (forward-ferry ferry* val)]
      [else (move-ferry ferry* act val)])))

(define (manhattan-distance ferry*)
  (match-let ([(ferry facing X Y) ferry*])
    (+ (abs X) (abs Y))))

;; Data
(define data
  (~>> "input/2020-12.txt"
       file->vector-of-char))

(define test
  (~>> "F10
N3
F7
R90
F11
"
       test->vector-of-char))


;; Puzzle
(display "test 1: ")
(~>> test
     (map string->instruction)
     (foldl update-ferry (ferry 'E 0 0))
     manhattan-distance)

(display "one: ")
(~>> data
     (map string->instruction)
     (foldl update-ferry (ferry 'E 0 0))
     manhattan-distance)

;(display "test 2: ")
;(~>> test)
;
;(display "two: ")
;(~>> data)

;; Unit Test
