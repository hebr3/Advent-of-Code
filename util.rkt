#lang racket/base
(require racket/string)

;; Displays the expression before the value of the expression
;; (-> expr expr)
(define-syntax-rule (tap expr)
  (begin
    (displayln (format "~a: ~a" 'expr expr))
    expr))

;; takes a filename and returns the file contents as a String
;; (-> String String)
(define (input->data filename)
  (read-line (open-input-file filename #:mode 'text) 'return-linefeed))
  
;; (-> String (Listof String))
(define (parse-lines str)
  (string-split str "\n"))

;; (-> String (Listof (Listof String)))
(define (parse-blocks str)
  (let ([blocks (string-split str "\n\n")])
    (map parse-lines blocks)))

;; (-> String (Listof Number))
(define (son-parse str px)
  (map string->number (string-split str #px",| ")))

;; (-> (Listof String) (λ (c x y)))
(define (los-parse los fn)
  (for ([str los][y (in-naturals)])
    (for ([ch str][x (in-naturals)])
      (fn ch x y))))

;; (-> (Listof String) (-> Char Integer Integer F) (Listof (Listof F)))
(define (los-parse->loloF los fn)
  (for/list ([str los][y (in-naturals)])
    (for/list ([ch str][x (in-naturals)])
      (fn ch x y))))

(struct point [x y] #:transparent)
(struct point-3d [x y z] #:transparent)

(provide tap
         input->data
         parse-lines
         parse-blocks
         son-parse
         los-parse
         los-parse->loloF
         point
         point-3d)

;; Examples
;; (let* ([input "1,2,3,4"]
;;        [px #px","])
;;   (son-parse input px))
;; 
;; (let* ([input "1 2 3 4"]
;;        [px #px""])
;;   (son-parse input px))
;; 
;; (let* ([input "123\n456\n789"]
;;        [los (string-split input "\n")]
;;        [H (make-hash)]
;;        [fn (λ (c x y) (hash-set! H (point x y) c))])
;;   (los-parse los fn)
;;   H)
;; 
;; (let* ([input "123\n456\n789\nabc"]
;;        [los (string-split input "\n")]
;;        [fn (λ (c x y) (list c (point x y)))])
;;   (los-parse->loloF los fn))

