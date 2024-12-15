#lang racket/base
(require racket/string)

(provide input->data)

(define (input->data filename)
  (read-line (open-input-file filename #:mode 'text) 'return-linefeed))