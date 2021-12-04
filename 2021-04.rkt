#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2021-04.txt"
      open-input-file
      (read-line _ 'return)
      (string-split _ "\n")))

(define (group-boards L)
  (for/list ([i (/ (length L) 5)])
    (~> (for/list ([j 5]) (list-ref L (+ j (* 5 i))))
        string-join
        string-normalize-spaces
        (string-split _ " ")
        (map string->number _))))

(define (format-data L)
  (let ([random-order (map string->number (string-split (car L) ","))])
    (~> (cdr L)
        (filter non-empty-string? _)
        group-boards
        (cons random-order _))))

(define test
  (list '(7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1)
        '(22 13 17 11 0 8 2 23 4 24 21 9 14 16 7 6 10 3 18 5 1 12 20 15 19)
        '(3 15 0 2 22 9 18 13 17 5 19 8 7 25 23 20 11 10 24 4 14 21 16 12 6)
        '(14 21 17 24 4 10 16 15 9 19 18 8 23 26 20 22 11 13 6 5 2 0 12 3 7)))

;(format-data data)

(define (winning-board? L)
  (or (for/or ([i 5])
        (for/and ([j 5])
          (zero? (list-ref L (+ i (* 5 j))))))
      (for/or ([i 5])
        (for/and ([j 5])
          (zero? (list-ref L (+ (* 5 i) j)))))))

(define winner0 '(0 0 0 0 0 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5))
(define winner1 '(1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 0 0 0 0 0))
(define winner2 '(0 2 3 4 5 0 2 3 4 5 0 2 3 4 5 0 2 3 4 5 0 2 3 4 5))
(define winner3 '(1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0)) 

;(winning-board? (cadr test))
;(winning-board? winner0)
;(winning-board? winner1)
;(winning-board? winner2)
;(winning-board? winner3)

(define (update-board L num)
  (for/list ([i L]) (if (= i num) 0 i)))

;(cadr test)
;(update-board (cadr test) 7)
;(update-board (update-board (cadr test) 7) 4)
;(update-board (update-board (update-board (cadr test) 7) 4) 9)

(define (check-boards LoB LoN)
  (let ([new-boards (map (λ (board) (update-board board (car LoN))) LoB)])
    (cond
      [(not (empty? (filter winning-board? new-boards)))
       (list (car LoN) (car (filter winning-board? new-boards)))]
      [else
       (check-boards new-boards (cdr LoN))])))

(match-let ([(list num L) (check-boards (cdr test) (car test))])
  (* num (apply + L)))

(match-let ([(list num L) (check-boards (cdr (format-data data)) (car (format-data data)))])
  (* num (apply + L)))