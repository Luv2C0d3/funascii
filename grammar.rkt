#lang ragg
;; I copied this from the ragg tutorials, see https://docs.racket-lang.org/ragg/
;;
;; See: http://stackoverflow.com/questions/12345647/rewrite-this-script-by-designing-an-interpreter-in-racket
;;

drawing: rows*
rows: repeat chunk+ ";"
repeat: INTEGER
chunk: INTEGER STRING
