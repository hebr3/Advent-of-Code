#lang racket

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

(define data (input->data "input/2024-09.txt"))

(define test "2333133121414131402")

(define (char->number c)
  (string->number (string c)))

(define (first-empty-index HT)
   (for/or ([i (in-naturals)])
     (and (not (hash-has-key? HT i))
          i)))

(define (find-last-index HT)
  (first (sort (hash-keys HT) >)))

(define (checksum HT)
  (for/sum ([i (hash-keys HT)])
    (* i (hash-ref HT i))))

(define (part-A L)
  (define memory (make-hash))
  (let ([idx 0][id 0])
    (for ([ch (first (string-split L))][i (in-naturals)])
      (cond
        [(even? i)
         (for ([n (char->number ch)])
           (hash-set! memory idx id)
           (set! idx (add1 idx)))
         (set! id (add1 id))]
        [(odd? i)
         (for ([n (char->number ch)])
             (set! idx (add1 idx)))]
        [else (println i)])))

  (define (iter)
    (let ([empty-idx (first-empty-index memory)]
          [last-idx (find-last-index memory)])
      (when (< empty-idx last-idx)
        (hash-set! memory empty-idx (hash-ref memory last-idx))
        (hash-remove! memory last-idx)
        (iter))))
  (iter)

  (checksum memory))
          

(part-A test)
;(part-A data)

;;

(struct FILE [id size idx] #:transparent)

(define (part-B L)
  (define files (make-hash))
  (define free-memory (make-hash))
  (let ([idx 0][id 0])
    (for ([ch (first (string-split L))][i (in-naturals)])
      (cond
        [(even? i)
         (hash-set! files idx (FILE id (char->number ch) idx))
         (set! idx (+ idx (char->number ch)))
         (set! id (add1 id))]
        [(odd? i)
         (when (not (zero? (char->number ch)))
           (hash-set! free-memory idx (char->number ch)))
         (set! idx (+ idx (char->number ch)))]
        [else (println i)])))

  (define (insert-file file)
    (match-let ([(FILE file-id file-size file-idx) file])
      (define free-memory-assoc (sort (hash->list free-memory) #:key car <))
      (let ([done #f])
        (for ([fm free-memory-assoc])
          (match-let ([(cons fm-id fm-size) fm])
            (cond
              [done 'done]
              [(and (<= file-size fm-size) (< fm-id file-idx))
               (hash-set! files fm-id (FILE file-id file-size fm-id))
               (hash-remove! files file-idx)
               (hash-remove! free-memory fm-id)
               (hash-set! free-memory (+ fm-id file-size) (- fm-size file-size))
               (set! done #t)]
              [else 'skip]))))))

  (for ([i (sort (hash-keys files) >)])
    (insert-file (hash-ref files i)))

  (define (checksum)
    (for/sum ([i (hash-keys files)])
      (match-let ([(FILE id size idx) (hash-ref files i)])
        (for/sum ([j size])
          (* id (+ j idx))))))
  (checksum))

(part-B test)
(part-B data)
