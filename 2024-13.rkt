#lang racket

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

(define data (input->data "input/2024-13.txt"))

(define test "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
")

(struct machine [ax ay bx by tx ty] #:transparent)

(define (string->machine str)
  (define pattern #px".+X\\+(\\d+), Y\\+(\\d+).+X\\+(\\d+).+Y\\+(\\d+).+X=(\\d+).+Y=(\\d+)")
  (match-let ([(list ax ay bx by tx ty)
               (map (λ (x) (string->number x)) (rest (regexp-match pattern str)))])
    (machine ax ay bx by tx ty)))

(define (reachable m)
  (match-let ([(machine ax ay bx by tx ty) m])
    (define det (- (* ax by) (* ay bx)))
    (define a (/ (- (* tx by) (* ty bx)) det))
    (define b (/ (- (* tx ay) (* ty ax)) det))
    (list a b)))

(define (cost r)
  (match-let ([(list a b) r])
    (if (and (integer? a) (integer? b))
        (+ (* 3 (abs a)) (abs b))
        0)))

(define (part-A L)
  (define blocks (string-split L "\n\n"))
  (define machines (map string->machine blocks))
  (define machine-calculations (map reachable machines))
  (for/sum ([result machine-calculations])
    (cost result)))

(part-A test)
(part-A data)

;;

(define (update-machine m)
  (match-let ([(machine ax ay bx by tx ty) m])
    (machine ax ay bx by (+ 10000000000000 tx) (+ 10000000000000 ty))))

(define (part-B L)
  (define blocks (string-split L "\n\n"))
  (define machines (map string->machine blocks))
  (define updated-machines (map update-machine machines))
  (define machine-calculations (map reachable updated-machines))
  (for/sum ([result machine-calculations])
    (cost result)))

(part-B test)
(part-B data)
