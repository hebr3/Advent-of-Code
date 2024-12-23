#lang racket

(define (input->data filename)
  (with-input-from-file filename
    (λ () (port->string (current-input-port)))))

;; Test data
(define data (input->data "input/2024-19.txt"))

(define test "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
")

;; Core word-break logic
(define (can-build-string? target substrings)
  
  ;; Helper to check if we can build string up to position n
  (define (can-build-to? n)
    (cond
      [(zero? n) #t]  ; Empty string can always be built
      [else
       (ormap
        (λ (substr)
          (let ([substr-len (string-length substr)])
            (and (<= substr-len n)
                 (can-build-to? (- n substr-len))
                 (string=? substr
                          (substring target 
                                   (- n substr-len)
                                   n)))))
        substrings)]))
  
  ;; Memoize the recursive function for efficiency
  (define memo (make-hash))
  (define (memo-can-build-to? n)
    (hash-ref! memo n
               (λ () (can-build-to? n))))
  
  (memo-can-build-to? (string-length target)))

(define (can-build? target los)
  (define N (string-length target))
  (define dp (make-hash))
  (hash-set! dp 0 #t)

  (for ([i (add1 N)])
    (for ([s los])
      (define s-len (string-length s))
      (cond
        [(< i s-len) 'next]
        [else
         (define start (- i s-len))
         (cond
           [(and (hash-has-key? dp start)
                 (string=? (substring target start i)))
            (hash-set! dp i #t)]
           [else 'next])])))
  (hash-ref dp N))

;; Main Function
(define (part-A input)
  (define blocks (string-split input "\n\n"))
  (define towels (string-split (first blocks) ", "))
  (define designs (string-split (second blocks)))
  (for/list ([design designs])
    (can-build-string? design towels)))

(count identity (part-A test))
(count identity (part-A data))

;;

;; Main function using functional approach
(define (count-string-combinations target-string substrings)
  ;; Helper to check if str2 is a suffix of str1
  (define (suffix? str1 str2)
    (let ([len1 (string-length str1)]
          [len2 (string-length str2)])
      (and (>= len1 len2)
           (string=? str2 (substring str1 (- len1 len2))))))
  
  ;; Helper to get all possible suffixes that match our substrings
  (define (matching-suffixes str)
    (filter (λ (substr) (suffix? str substr)) 
            substrings))
  
  ;; Memoization hash table
  (define memo (make-hash))
  
  ;; Core recursive function with memoization
  (define (count-combinations str)
    (cond
      ;; Base cases
      [(string=? str "") 1]  ; Empty string can be formed in one way
      [(hash-has-key? memo str) (hash-ref memo str)]  ; Return memoized result
      [else
       (let ([result
              (for/sum ([suffix (matching-suffixes str)])
                (count-combinations
                 (substring str 
                           0 
                           (- (string-length str) 
                              (string-length suffix)))))])
         (hash-set! memo str result)
         result)]))
  
  (count-combinations target-string))

(define (part-B input)
  (define blocks (string-split input "\n\n"))
  (define towels (string-split (first blocks) ", "))
  (define designs (string-split (second blocks)))
  (for/sum ([design designs])
    (count-string-combinations design towels)))

(part-B test)
(part-B data)
