#lang racket
(require threading)
(require file/md5)

(define (build-password id)
  (define prefix (string->bytes/utf-8 id))
  (define (iter c acc)
    (when (zero? (modulo c 1000000))
      (println (list c acc)))
    (let* ([postfix (string->bytes/utf-8 (number->string c))]
           [byte-str (bytes-append prefix postfix)]
           [md5-hash (md5 byte-str)]
           [str (bytes->string/utf-8 md5-hash)])
      (cond
        [(< 7 (string-length acc)) acc]
        [(string-prefix? str "00000")
         (iter (add1 c) (string-join (list acc (substring str 5 6)) ""))]
        [else
         (iter (add1 c) acc)])))
  (iter 0 ""))

;(build-password "abc")
;(build-password "ffykfhsq")

(define (list->password los)
  (map (λ (s) (cons (string->number (substring s 5 6)) (substring s 6 7))) los))

(define (build-password2 id)
  (define prefix (string->bytes/utf-8 id))
  (define keys (make-hash))
  (define (iter c)
    ;(when (zero? (modulo c 1000000))
    ;  (println (list c keys)))
    (let* ([postfix (string->bytes/utf-8 (number->string c))]
           [byte-str (bytes-append prefix postfix)]
           [md5-hash (md5 byte-str)]
           [str (bytes->string/utf-8 md5-hash)])
      (cond
        [(< 7 (length (hash-keys keys))) keys]
        [(and (string-prefix? str "00000")
              (regexp-match #px"0|1|2|3|4|5|6|7" (substring str 5 6))
              (not (hash-ref keys (substring str 5 6) #f)))
         (hash-set! keys (substring str 5 6) (substring str 6 7))
         (iter (add1 c))]
        [else
         (iter (add1 c))])))
  (iter 0)
  (~> keys
      hash->list
      (sort _ #:key car string<?)
      (map cdr _)
      (string-join _ "")))

(time (build-password2 "abc"))
;(time (build-password2 "ffykfhsq"))

;; work
;(define prefix (string->bytes/utf-8 "abc"))
;prefix

;(define postfix (string->bytes/utf-8 (number->string 3231929)))
;postfix

;(define byte-str (bytes-append prefix postfix))
;byte-str

;(define md5-hash (md5 byte-str))
;md5-hash

;(define str (bytes->string/utf-8 md5-hash))
;str

;(string-prefix? str "00000")
