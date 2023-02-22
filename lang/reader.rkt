;; This first line says where the two key parts of the language are
;; the reader and the expander. In this language there's also an extra
;; coloring thing which is used by racket.
#lang s-exp syntax/module-reader
funascii/semantics
#:read my-read
#:read-syntax my-read-syntax
#:info my-get-info
#:whole-body-readers? #t

(require funascii/lexer
         funascii/grammar)

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src ip)
  (list (parse src (tokenize ip))))

(define (my-get-info key default default-filter)
  (case key
    [(color-lexer)
     (dynamic-require 'syntax-color/default-lexer 'default-lexer)]
    [else
     (default-filter key default)]))
