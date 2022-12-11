#lang racket
(require threading)
(require racket/match)

(define data
  (~> "input/2022-11.txt"
      open-input-file
      (read-line _ 'return)
      ))

(define test "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

;;---

(struct monkey [id oper divi target-1 target-2] #:transparent)

(define (parse-monkey str ht-m ht-items ht-count)
  (match-let ([(list A B C D E F) (string-split str "\n")])
    ;(println (list A B C D E F))
    (let ([monkey-id (string->number (substring A 7 8))]
          [items (map string->number (string-split (substring B 18) ", "))]
          [oper (substring C 23)]
          [divi (string->number (substring D 21))]
          [target-1 (string->number (substring E 29))]
          [target-2 (string->number (substring F 30))])
      ;(println (format "monkey-id: ~a" monkey-id))
      ;(println (format "items: ~a" items))
      ;(println (format "oper: ~a" oper))
      ;(println (format "divi: ~a" divi))
      ;(println (format "target-2: ~a" target-1))
      ;(println (format "target-1: ~a" target-2))
      (hash-set! ht-m monkey-id (monkey monkey-id oper divi target-1 target-2))
      (hash-set! ht-items monkey-id items)
      (hash-set! ht-count monkey-id 0))))

(define (calc-mon mon val)
  ;(println (format "mon: ~a, val: ~a" mon val))
  (define (floor3 val)
    (floor (/ val 3)))
  (match-let ([(list op num) (string-split (monkey-oper mon) " ")])
    (cond
      [(and (string=? "*" op) (string=? "old" num)) (floor3 (* val val))]
      [(string=? "*" op) (floor3 (* val (string->number num)))]
      [(and (string=? "old" num) (floor3 (+ val val)))]
      [else (floor3 (+ val (string->number num)))])))

(define (round mon HT-I HT-C)
  (let ([held (hash-ref HT-I (monkey-id mon))])
    ;(println (format "monkey: ~a, held: ~a" mon held))
    (hash-set! HT-C (monkey-id mon) (+ (length held) (hash-ref HT-C (monkey-id mon))))
    (for ([i held])
      (define newVal (calc-mon mon i))
      ;(println (format "newVal: ~a" newVal))
      (define t1 (monkey-target-1 mon))
      (define t2 (monkey-target-2 mon))
      (if (zero? (modulo newVal (monkey-divi mon)))
          (hash-set! HT-I t1 (flatten (list (hash-ref HT-I t1) newVal)))
          (hash-set! HT-I t2 (flatten (list (hash-ref HT-I t2) newVal)))))
          ;(println (format "t1: ~a, ->t1: ~a" t1 (flatten (list (hash-ref HT-I t1) (list newVal)))))
          ;(println (format "t2: ~a, ->t2: ~a" t2 (flatten (list (hash-ref HT-I t2) (list newVal))))))) 
    (hash-set! HT-I (monkey-id mon) '())))


(define (part-A L)
  (define HT-M (make-hash))
  (define HT-I (make-hash))
  (define HT-C (make-hash))
  (~>> L
       (string-split _ "\n\n")
       (for-each (λ (mon) (parse-monkey mon HT-M HT-I HT-C))))
  ;(println HT-M)
  ;(println HT-I)
  (for ([i 20])
    (for ([m (hash-keys HT-I)])
      (round (hash-ref HT-M m) HT-I HT-C)))
    ;(println HT-I))
  (~> (for/list ([i (hash-keys HT-C)])
        (hash-ref HT-C i))
      (sort >)
      (take 2)
      (apply * _)))

;(part-A test)
;(part-A data)

;;---

(define (calc-mon2 mon val)
  ;(println (format "mon: ~a, val: ~a" mon val))
  (match-let ([(list op num) (string-split (monkey-oper mon) " ")])
    (cond
      [(and (string=? "*" op) (string=? "old" num)) (* val val)]
      [(string=? "*" op) (* val (string->number num))]
      [(and (string=? "old" num) (+ val val))]
      [else (+ val (string->number num))])))

(define (round2 mon HT-I HT-C)
  (let ([held (hash-ref HT-I (monkey-id mon))])
    ;(println (format "monkey: ~a, held: ~a" mon held))
    (hash-set! HT-C (monkey-id mon) (+ (length held) (hash-ref HT-C (monkey-id mon))))
    (for ([i held])
      (define newVal (calc-mon2 mon i))
      ;(println (format "newVal: ~a" newVal))
      (define t1 (monkey-target-1 mon))
      (define t2 (monkey-target-2 mon))
      (if (zero? (modulo newVal (monkey-divi mon)))
          (hash-set! HT-I t1 (flatten (list (hash-ref HT-I t1) newVal)))
          (hash-set! HT-I t2 (flatten (list (hash-ref HT-I t2) newVal)))))
          ;(println (format "t1: ~a, ->t1: ~a" t1 (flatten (list (hash-ref HT-I t1) (list newVal)))))
          ;(println (format "t2: ~a, ->t2: ~a" t2 (flatten (list (hash-ref HT-I t2) (list newVal))))))) 
    (hash-set! HT-I (monkey-id mon) '())))


(define (part-B L)
  (define HT-M (make-hash))
  (define HT-I (make-hash))
  (define HT-C (make-hash))
  (define HT-mod (make-hash))
  (~>> L
       (string-split _ "\n\n")
       (for-each (λ (mon) (parse-monkey mon HT-M HT-I HT-C))))
  ;(println HT-M)
  ;(println HT-I)
  (for ([i 100])
    (for ([m (hash-keys HT-I)])
      (round2 (hash-ref HT-M m) HT-I HT-C)))
    ;(println (format "HT-I: ~a" HT-I)))
  ;(println (format "HT-C: ~a" HT-C))
  (~> (for/list ([i (hash-keys HT-C)])
        (hash-ref HT-C i))
      (sort >)
      (take 2)
      (apply * _)))

(part-B test)
;(part-B data)
