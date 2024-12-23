#lang racket

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

(define data (input->data "input/2024-06.txt"))

(define test "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(define (get-bounds L)
  (let ([lines (string-split L "\n")])
    (list (string-length (first lines))
          (length lines))))

(define (get-obstructions L)
  (let ([OBSTRUCTIONS (mutable-set)]
        [rows (string-split L "\n")])
    (for ([row rows][r (in-naturals)])
      (for ([ch row][c (in-naturals)])
        (when (char=? #\# ch)
          (set-add! OBSTRUCTIONS (list c r)))))
    OBSTRUCTIONS))

(define (get-guard L)
  (let ([rows (string-split L "\n")])
    (for/or ([row rows][r (in-naturals)])
      (for/or ([ch row][c (in-naturals)])
        (and (char=? #\^ ch)
             (list c r 'north))))))

(define (rotate guard)
  (match guard
    [(list c r 'north) (list c r 'east)]
    [(list c r 'east) (list c r 'south)]
    [(list c r 'south) (list c r 'west)]
    [(list c r 'west) (list c r 'north)]))

(define (update-guard guard obstructions positions bounds)
  ;  (println (list guard obstructions positions bounds))
  (match-let ([(list c r dir) guard]
              [(list X Y) bounds])
    (cond
      [(or (< c 0) (< X c) (< r 0) (< Y r))
       positions]
      [(eq? 'north dir)
       (if (set-member? obstructions (list c (sub1 r)))
           (update-guard (rotate guard) obstructions positions bounds)
           (begin
             (set-add! positions (list c r))
             (update-guard (list c (sub1 r) dir) obstructions positions bounds)))]
      [(eq? 'east dir)
       (if (set-member? obstructions (list (add1 c) r))
           (update-guard (rotate guard) obstructions positions bounds)
           (begin
             (set-add! positions (list c r))
             (update-guard (list (add1 c) r dir) obstructions positions bounds)))]
      [(eq? 'south dir)
       (if (set-member? obstructions (list c (add1 r)))
           (update-guard (rotate guard) obstructions positions bounds)
           (begin
             (set-add! positions (list c r))
             (update-guard (list c (add1 r) dir) obstructions positions bounds)))]
      [(eq? 'west dir)
       (if (set-member? obstructions (list (sub1 c) r))
           (update-guard (rotate guard) obstructions positions bounds)
           (begin
             (set-add! positions (list c r))
             (update-guard (list (sub1 c) r dir) obstructions positions bounds)))]
      [else
       positions])))
          

(define (part-A L)
  (let ([BOUNDS (get-bounds L)]
        [OBSTRUCTIONS (get-obstructions L)]
        [GUARD (get-guard L)]
        [POSITIONS (mutable-set)])
    (sub1 (set-count (update-guard GUARD OBSTRUCTIONS POSITIONS BOUNDS)))))

(part-A test)
(part-A data)

;;

(define (generate-boards L)
  (for/list ([i (string-length L)])
    (let ([copy (string-copy L)])
      (when (char=? #\. (string-ref L i)) 
        (string-set! copy i #\#))
      copy)))

(define (update-guard-2 guard obstructions positions bounds)
;  (println (list guard obstructions positions bounds))
  (match-let ([(list c r dir) guard]
              [(list X Y) bounds])
    (cond
      [(or (< c 0) (< X c) (< r 0) (< Y r))
       'non-loop]
      [(set-member? positions guard)
       'loop]
      [(eq? 'north dir)
       (if (set-member? obstructions (list c (sub1 r)))
           (update-guard-2 (rotate guard) obstructions positions bounds)
           (begin
             (set-add! positions guard)
             (update-guard-2 (list c (sub1 r) dir) obstructions positions bounds)))]
      [(eq? 'east dir)
       (if (set-member? obstructions (list (add1 c) r))
           (update-guard-2 (rotate guard) obstructions positions bounds)
           (begin
             (set-add! positions guard)
             (update-guard-2 (list (add1 c) r dir) obstructions positions bounds)))]
      [(eq? 'south dir)
       (if (set-member? obstructions (list c (add1 r)))
           (update-guard-2 (rotate guard) obstructions positions bounds)
           (begin
             (set-add! positions guard)
             (update-guard-2 (list c (add1 r) dir) obstructions positions bounds)))]
      [(eq? 'west dir)
       (if (set-member? obstructions (list (sub1 c) r))
           (update-guard-2 (rotate guard) obstructions positions bounds)
           (begin
             (set-add! positions guard)
             (update-guard-2 (list (sub1 c) r dir) obstructions positions bounds)))]
      [else
       'error])))

(define (part-B L)
  (let ([BOARDS (generate-boards L)])
;    (for ([b BOARDS]) (println b))
    (count (λ (x) (eq? x 'loop))
           (for/list ([board BOARDS])
             (let ([BOUNDS (get-bounds board)]
                   [OBSTRUCTIONS (get-obstructions board)]
                   [GUARD (get-guard board)]
                   [POSITIONS (mutable-set)])
               (update-guard-2 GUARD OBSTRUCTIONS POSITIONS BOUNDS))))))

(part-B test)
(part-B data)
