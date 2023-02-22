#lang racket/base

(require "grammar.rkt" "lexer.rkt")
#|

A custom minimal reader for this language.

Every read-syntax has one job: to return syntax, which in this case
is the code describing some instructions for a drawing.

Racket will replace the orig­inal source code with this module,
which will invoke the expander for our language, trig­gering the
full expan­sion of he module.

After that, the module will be eval­u­ated normally by the Racket
inter­preter.

|#
(define (read-syntax path port)
  (define parse-tree (parse path (tokenize port)))
  #`(module funascii-parser-mod funascii/interpret
      #,parse-tree))
(module+ reader (provide read-syntax))
#|
 The minimal expander which will in this case, just pass the parse
 tree as is to the interpreter function
|#
(require (for-syntax racket/base syntax/parse))
(define-syntax (interpreter-mod stx)
  (syntax-parse stx
    [((~literal interpreter-mod) PARSE-TREE)
     #'(#%module-begin
        (interpret-drawing #'PARSE-TREE))
     ]
    ))

#|
The interpreter functions
See: https://docs.racket-lang.org/ragg/#%28part._.From_parsing_to_interpretation%29
|#

(require syntax/parse)
(define (interpret-drawing drawing-stx)
  (syntax-parse drawing-stx
    [({~literal drawing} rows-stxs ...)

     (for ([rows-stx (syntax->list #'(rows-stxs ...))])
       (interpret-rows rows-stx)
       )])

  )

(define (interpret-rows rows-stx)
  (syntax-parse rows-stx
    [({~literal rows}
      ({~literal repeat} repeat-number)
      chunks ... ";")

     (for ([i (syntax-e #'repeat-number)])
       (for ([chunk-stx (syntax->list #'(chunks ...))])
         (interpret-chunk chunk-stx))
       (newline))]))

(define (interpret-chunk chunk-stx)
  (syntax-parse chunk-stx
    [({~literal chunk} chunk-size chunk-string)

     (for ([k (syntax-e #'chunk-size)])
       (display (syntax-e #'chunk-string)))]))

#|
We only need to provide interpret-drawing as it will
be the only function, other than the #%module-begin which
racket will directly call.
|#
(provide interpret-drawing
         (rename-out [interpreter-mod #%module-begin]))