#lang racket

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
  #`(module funascii-parser-mod funascii/interpret-3
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
        (interpret-drawing 'PARSE-TREE))
     ]
    )
  )

(define (interpret-drawing drawing-el)
  (cond
    [(eq? (car drawing-el) 'drawing)
     (let [(flatlist (flatten (int-rows-list (cdr drawing-el))))]
       (display (apply string-append flatlist))
       )]
    )
  )
  
(define (int-rows-list rows-list)
  (cond
    [(null? rows-list) '()]
    [else
     (cons (int-rows-el (car rows-list))
           (int-rows-list (cdr rows-list)))
     ]
    )
  )

(define (int-rows-el rows-el)
  (let [(row-repeat (cadadr rows-el))
        (chunks (cddr rows-el))]
    (int-row row-repeat chunks)
    )
  )

(define (int-row row-repeat chunks)
  (cond [(eq? row-repeat 0) '()]
        [else
         (cons (int-chunks chunks)
               (int-row (- row-repeat 1) chunks))])
  )

(define (int-chunks chunks)
  (cond
    [(eq? ";" (car chunks)) '("\n")]
    [else
     (cons (int-chunk (car chunks))
           (int-chunks (cdr chunks)))])
  )

(define (int-chunk chunk)
  (let [(chunk-repeat (cadr chunk))
        (chunk-char (caddr chunk))]
    (display-char chunk-char chunk-repeat)
    )
  )

(define (display-char char repeat)
  (cond
    [(eq? 0 repeat) '()]
    [else
     (cons char
           (display-char char (- repeat 1)))
     ]
    )
  )

#|
We only need to provide interpret-drawing as it will
be the only function, other than the #%module-begin which
racket will directly call.
|#
(provide interpret-drawing
         (rename-out [interpreter-mod #%module-begin]))