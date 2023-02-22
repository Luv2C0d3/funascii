# Creating a minimal language in racket
This project is a modified version of the examples described in 
[ragg: a Racket AST Generator Generator](https://docs.racket-lang.org/ragg/)
which are in the project [Simple line drawing](https://github.com/dyoo/ragg/tree/master/ragg/examples/simple-line-drawing) by  Danny Yoo. 

# Purpose of this project
I was having trouble understanding (1) what is the **minimum** amount of boilerplate needed to create a language in racket and (2) the difference
between when things are interpreted and when things are compiled in racket. 

The `ragg` guide assumes you understand how to create a language, but the concept, was not that clear to me. Add to that, the different racket functions/macros, though very handy, were creating a layer of magic which in my case was making things harder to understand.

The result of my investigations is this project
where I included the code for the compiler exactly as it is in Danny Yoo's original simple
line drawing, but I also created several languages that interpret things using both the macros
or simple racket function calls.

> Note: To be precise, I modified a little bit the lexer to be able to draw something a bit more fun. Check [sample-2.rkt](./examples/sample-2.rkt).

## Install things

```
# Clone this repo then
$ cd funascii
$ raco pkg install
```
After installing, the project should show as a package in racket:
```
$ raco pkg show funascii
Installation-wide:
 [none]
User-specific for installation "8.7":
 Package  Checksum                Source
 funascii                            link...languages\funascii
```
## Run something

Examples are in the `examples` folder

```
$ racket examples/sample-1.rkt
```

## Check how interpret is different than compile
For any example, in the first line which reads:

```
#lang funascii
```
Replace it by this line to interpret:
```
#lang funascii/interpret
```
> Note: There are several interpreters, so you may want to play changing the interpret line
by `interpret-2` or `interpret-3`.

## Use a parse-only language to check the output of the parser
There is also a language, which simply shows the parse tree, to use it change the `#lang` line to:
```
#lang funascii/parse-only
```

# Let's start from the beginning
Or rather the "start from the end" as really what I was doing through the project is de-constructing the advanced compile sample into something easier for me to assimilate from generic scheme language knowledge.

## So, what do I need for a language in racket?

* Register the project as per above.
* A reader and an expander.
* A lang folder with a reader.rkt inside that ties together all the parts.

**In a bit more detail**:

* The reader may include a parser and a tokenizer to massage the input to obtain an a syntax object (an enriched s-expression) out of the reader.
  * The tokenizer, in the case of `ragg`, is a convenience in charge of creating tokens, the minimal lexical units seen, such that ragg instead of seeing "foo" as "f" "o" "o" and thus having to assemble from there the concept of an identifier, sees a token of type IDENTIFIER with value "foo".
  * The parser, need not be `ragg`, it could be just about anything that massages syntax through the reader stages to get to a syntax object which can be further processed by the expander. `ragg` however is convenient, because it offers a DSL to write a BNF like grammar for a language.
  
* The lang folder is what was used in the original [Simple line drawing](https://github.com/dyoo/ragg/tree/master/ragg/examples/simple-line-drawing) project, but there is an alternate way to define languages, which is what I used for all the additional language flavors in this project.

## First step: Simple line drawing using original files
This first step should really be the last one shown as it is the most complex one. I put it at the beginning, because it is the one that is used and referenced in the racket guide.

**The files needed for this one are**:
```
.
├── grammar.rkt
├── lang
│   └── reader.rkt
├── lexer.rkt
└── semantics.rkt
```
The files `grammar.rkt` and `lexer.rkt` are shared between the compiled language, this one, and the 
interpreted language versions I added.

As to the magic to turn the project into a language, it is the responsibility of the reader.rkt file in the lang folder, specifically these lines tie everything together:
```
#lang s-exp syntax/module-reader
funascii/semantics
#:read my-read
#:read-syntax my-read-syntax
#:info my-get-info
#:whole-body-readers? #t
```


## Second step: Simple line drawing interpreter
Going back, in the ragg guide section [2.6 From interpretation to compilation](https://docs.racket-lang.org/ragg/#%28part._.From_interpretation_to_compilation%29), there is a brief mention in parentheses which reads _"(We could just resort to simply calling into the interpreter we just wrote up, but this section is meant to show that compilation is also viable.)"_. Now, the problem I was having is that the ragg guide is dealing with two concepts in a single step: 
1. Turning the code into a language and
2. Transforming interpreted code to compiled code.

This was just too much for me as I needed to separate things and understand how to create a language regardless of whether I would interpret or compile things. 

So, I set myself up to create a language, that would interpret things. The result is `#lang funascii/interpret` which uses the interpreter described in [2.5 From parsing to interpretation](https://docs.racket-lang.org/ragg/#%28part._.From_parsing_to_interpretation%29)

**The files for this language are**:
```
.
├── grammar.rkt
├── interpret.rkt
└── lexer.rkt
```
In order to test a file using this interpreter, modify one of the files
in the examples folder so that the first line reads:
```
#lang funascii/interpret
```
If you launch `DrRacket` and enter the
following in the definitions window it should show the result of interpreting:
```
#lang funascii/interpret

3 9 X;
6 3 b 3 X 3 b;
3 9 X;
```
### What is the magic needed to create a language in this case?
The file `interpret.rkt` has both the interpreter as well as the magic incantations needed to create an interpreted language, which are:
* A reader, provided via a `read_syntax` function, which in turn invokes the tokenizer and parser in sequence.
* An expander, provided via a `#%module-begin` macro.

Note that this is the recipe that I use in all the languages I created in this project.

## Going back: An interpreter with simple schemy racket + side effects
Going backwards, I wanted to check how feasible it would be to create a language using mostly generic scheme functions, without any need for fancy `syntax-parse` or similar constructs.

**The files for this language are**:
```
.
├── grammar.rkt
├── interpret-2.rkt
└──  lexer.rkt
```
In order to test a file using this interpreter, modify one of the files
in the examples folder so that the first line reads:
```
#lang funascii/interpret-2
```
As mentioned, this interpreter uses standard scheme as shown in the sample below:
```scheme
(define (interpret-drawing drawing-el)
  (cond
    [(eq? (car drawing-el) 'drawing)
     (int-rows-list (cdr drawing-el))]
    )
  )

(define (int-rows-list rows-list)
  (cond
    [(null? rows-list) (void)]
    [else
     (int-rows-el (car rows-list))
     (int-rows-list (cdr rows-list))
     ]
    )
  )
...
(define (display-char char repeat)
  (cond
    [(eq? 0 repeat) (void)]
    [else
     (display char)
     (display-char char (- repeat 1))
     ]
    )
  )

```

It's interesting to observe that even though this version uses standard syntax, it does so in an imperative way where the `display` call is done as part of the final tree walk step of the input s-expression. 


## One more: An interpreter with simple functional schemy racket
This final step is similar to the previous one, just more functional in that it composes the result as the output of functions and has a single print statement at the end.

**The files for this language are**:
```
.
├── grammar.rkt
├── interpret-3.rkt
└──  lexer.rkt
```
In order to test a file using this interpreter, modify one of the files
in the examples folder so that the first line reads:
```
#lang funascii/interpret-3
```
This interpreter uses functional coding style:
```scheme
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
...
```

# So what is the difference between interpretation and compilation?
The simplest answer is that in interpretation, the syntax object which results from the reader is fed as data to a function that interprets, whereas in compilation, racket, after performing macro transformations, expects actual function bindings present in the expander to match each of the first elements in the input syntax object.

An example will make this clearer. If you run this program:
```

```
The output that you get is:
```scheme
'(drawing 
  (rows (repeat 3) (chunk 9 "X") ";") 
  (rows (repeat 6) (chunk 3 " ") (chunk 3 "X") (chunk 3 " ") ";") 
  (rows (repeat 3) (chunk 9 "X") ";")
)
```
In the compiled case, which is in the file [semantics.rkt](./semantics.rkt), you can see that the first elements `drawing`, `rows` and `chunk` have actual bindings exported:
```scheme
...
;; Wire up the use of "drawing", "rows", and "chunk" to these
;; transformers:
(define-syntax drawing compile-drawing)
(define-syntax rows compile-rows)
(define-syntax chunk compile-chunk)
```

In the interpreted case, as in the case of interpret-* files, only the interpret-rows function needs to be provided:
```scheme
...
#|
We only need to provide interpret-drawing as it will
be the only function, other than the #%module-begin which
racket will directly call.
|#
(provide interpret-drawing
         (rename-out [interpreter-mod #%module-begin]))

```
> Note: Don't get confused by The `#%module-begin` above as that is needed for the expander in any language. The expander will need to have visibility to the `interpret-drawing` function to feed the input syntax object into it.

## Further inspecting the differences between interpretation and compilation
It's possible to see even in more detail the difference between interpretation and compilation by looking at the resulting [racket fully expanded syntax](https://docs.racket-lang.org/reference/syntax-model.html#%28part._fully-expanded%29) for each language. In order to do that, you can use the function expand directly. For your convenience I have included a little program in the utilities folder for that purpose. You use it by running from the project folder this line:
```bash
$ racket utilities/test-expand.rkt
```
A GUI will open and allow you to select a script which includes the `#lang` line for the language you're interested in.

### Watching macro expansion on compiled program
The following is an excerpt of running the program on [sample-1.rkt](./examples/sample-1.rkt) using
```scheme
#lang funascii
```
Returns this:
```bash
$ racket utilities/test-expand.rkt
Changing folder to 'C:\Users\...\Documents\racket-studies\languages\funascii\examples\'
Reading syntax for 'sample-1.rkt'
Input syntax (Unexpanded) out of reader:

'(module sample-1 funascii/semantics
   (#%module-begin
    (drawing
     (rows (repeat 3) (chunk 9 "X") ";")
     (rows (repeat 6) (chunk 3 " ") (chunk 3 "X") (chunk 3 " ") ";")
     (rows (repeat 3) (chunk 9 "X") ";"))))


Expanded input syntax:

'(module sample-1 funascii/semantics
   (#%module-begin
    (module configure-runtime '#%kernel
      (#%module-begin (#%require racket/runtime-config) (#%app configure '#f)))
    (#%app
     call-with-values
     (lambda ()
       (let-values (((start) '0) ((end) '3) ((inc) '1))
         (if (#%app variable-reference-from-unsafe? (#%variable-reference))
           (#%app void)
           (let-values () (#%app check-range start end inc)))
         (#%app
          (letrec-values (((for-loop)
                           (lambda (pos)
                             (if (#%app < pos end)
                               (let-values (((i) pos))
                                 (if '#t
                                   (let-values ((()
...
...                                                 (let-values ()
                                          values)))))
            for-loop)
          start)))
     print-values)
    (#%app call-with-values (lambda () (#%app void)) print-values)))

$
```
> Note: The entire macro expansion adds to 400+ lines of code.

### Watching macro expansion on interpreted program
Running the tool on the same script, [sample-1.rkt](./examples/sample-1.rkt) using
```scheme
#lang funascii/interpret-3
```
Returns this:
```bash
$ racket utilities/test-expand.rkt
Changing folder to 'C:\Users\....\Documents\funascii\examples\'
Reading syntax for 'sample-1.rkt'
Input syntax (Unexpanded) out of reader:

'(module funascii-parser-mod funascii/interpret-3
   (drawing
    (rows (repeat 3) (chunk 9 "X") ";")
    (rows (repeat 6) (chunk 3 " ") (chunk 3 "X") (chunk 3 " ") ";")
    (rows (repeat 3) (chunk 9 "X") ";")))


Expanded input syntax:

'(module funascii-parser-mod funascii/interpret-3
   (#%module-begin
    (module configure-runtime '#%kernel
      (#%module-begin (#%require racket/runtime-config) (#%app configure '#f)))
    (#%app
     call-with-values
     (lambda ()
       (#%app
        interpret-drawing
        '(drawing
          (rows (repeat 3) (chunk 9 "X") ";")
          (rows (repeat 6) (chunk 3 " ") (chunk 3 "X") (chunk 3 " ") ";")
          (rows (repeat 3) (chunk 9 "X") ";"))))
     print-values)))

$
```
As can be seen above, the expansion, just added `#%module-begin` and a couple more things, but it's roughly twice the size of the unexpanded syntax out of the reader.
