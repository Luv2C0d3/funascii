#lang racket
#|
The purpose of this file is to better understand how
macros are expanded.
The first variable in_s holds the results of
reading the input and transforming to s-exps,
every parentheses like [ and { are converted to regular (
without still expanding macros.

For now, copy this file to the folder where the language
in question is being developed.

Source where I got inspiration for this:
https://stackoverflow.com/a/37987172/1724025
|#

(require racket/gui)
(require dirname)
(require racket/pretty)

(define in_file (get-file))
(unless in_file
  (exit))

(define file_name (basename in_file))
(define file_folder (dirname in_file))
(current-directory file_folder)
(printf "Changing folder to '~a'\n" (current-directory))
(printf "Reading syntax for '~a'\n" file_name)
(read-accept-reader #t)
(define in_s (with-input-from-file in_file
               (λ () (read-syntax in_file))))
(printf "Input syntax (Unexpanded) out of reader:\n\n")
;(displayln in_s)
(pretty-print (syntax->datum in_s))

(printf "\n\nExpanded input syntax:\n\n")
(define expanded_s (parameterize ([current-namespace (make-base-namespace)]) (expand in_s)))
;(displayln expanded_s)
(pretty-print (syntax->datum expanded_s))
