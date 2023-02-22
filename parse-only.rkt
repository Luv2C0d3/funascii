#lang racket/base
(require "grammar.rkt" "lexer.rkt")
#|

A custom minimal reader for this language.

Racket will replace the orig­inal source code with the module
crafted by the reader. The new module, specifies the expander,
which happens to be this very file and needs to have a
#%module-begin which is expected of any expander.

|#
(define (read-syntax path port)
  (define parse-tree (parse path (tokenize port)))
  #`(module funascii-parser-mod funascii/parse-only
      #,parse-tree))
(module+ reader (provide read-syntax))

#|
The minimal expander returns only the parse tree, but quoted such
that it is not evaluated by racket.
|#
(require (for-syntax racket/base syntax/parse))
(define-syntax (parse-only-module stx)
  (syntax-parse stx
    [((~literal parse-only-module) PARSE-TREE)
     #'(#%module-begin
        ;; The key to parse-only is to return a quoted parse tree.
        'PARSE-TREE)
     ]
    [else
     (raise-syntax-error
      'parse-only-module
      (format "no matching case for calling pattern in: ~a"
              (syntax->datum stx)))]
    ))

(provide (rename-out [parse-only-module #%module-begin]))