#lang racket

(require rackunit)
(require racket/set)
(require racket/draw racket/gui)

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))
    #:mode 'text))

;; Test data
(define data (input->data "input/2025-09.txt"))

(define test "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

;; Helper Function
(struct point [x y] #:transparent)

(define (area p1 p2)
  (match-let ([(point x1 y1) p1]
              [(point x2 y2) p2])
    (* (add1 (abs (- x1 x2)))
       (add1 (abs (- y1 y2))))))

(define (calc-areas pairs)
  (for/list ([pair pairs])
    (match-let ([(list p1 p2) pair])
      (area p1 p2))))

(define (parse-input input)
  (for/list ([line (string-split input "\n")])
    (match-define (list x y) (string-split line ","))
    (point (string->number x) (string->number y))))

(define (polygon->bitmap pts width height
                         #:color [color "blue"]
                         #:brush-style [style 'solid])
  (define bmp (make-bitmap width height))
  (define dc  (new bitmap-dc% [bitmap bmp]))

  ;; Optional: clear background
  (send dc set-brush "white" 'solid)
  (send dc draw-rectangle 0 0 width height)

  ;; Create a path
  (define path (new dc-path%))

  ;; Move to first point
  (define first-pt (first pts))
  (send path move-to (point-x first-pt) (point-y first-pt))

  ;; Draw lines to remaining points
  (for ([pt (in-list (rest pts))])
    (send path line-to (point-x pt) (point-y pt)))

  ;; Close polygon
  (send path close)

  ;; Fill the polygon
  (send dc set-brush color style)
  (send dc draw-path path)

  bmp)

(define (scale-down points)
  (define max-x (apply max (map point-x points)))
  (define max-y (apply max (map point-y points)))
  (for/list ([p points])
    (match-define (point x y) p)
    (point (* 400 (/ x max-x)) (* 400 (/ y max-y)))))

;; Main Function
(define (part-A input)
  (define points (parse-input input))
  (define pairs (combinations points 2))
  (define areas (calc-areas pairs))
  (apply max areas))

(check-equal? (part-A test) 50)

(part-A data)

;;

(define (part-B input)
  (define points* (parse-input input))
  (define points (scale-down points*))
  (define xs (map point-x points))
  (define ys (map point-y points))
  (polygon->bitmap points 400 400))

(check-equal? (part-B test) 0)

(part-B data)
