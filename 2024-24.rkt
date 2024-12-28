#lang racket
(require data/queue)

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

;; Test data
(define data (input->data "input/2024-25.txt"))

(define test "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####
")

;; Structs

;; Helper
(define (key? str)
  (char=? #\. (string-ref str 0)))

(define (not-key? str)
  (not (key? str)))

(define (key->heights str)
  (define rows (string-split str "\n"))
  (for/list ([x (string-length (first rows))])
    (for/or ([row rows][y (in-naturals)])
      (and (char=? #\# (string-ref row x))
           (- 6 y)))))

(define (schema->heights str)
  (define rows (string-split str "\n"))
  (for/list ([x (string-length (first rows))])
    (for/or ([row rows][y (in-naturals)])
      (and (char=? #\. (string-ref row x))
           (sub1 y)))))

(define (key-fits key schema)
  (for/and ([k key][s schema]) (< k (- 6 s))))

;; Main Function

(define (part-A input)
  (define blocks (string-split input "\n\n"))
  (define schemas (map schema->heights (filter not-key? blocks)))
  (define keys (map key->heights (filter key? blocks)))
  (for/sum ([schema schemas])
    (for/sum ([key keys])
      (if (key-fits key schema)
          1 0))))

(part-A test)
(part-A data)
