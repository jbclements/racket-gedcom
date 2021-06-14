#lang racket

;; I hate to say it but I think lex/yacc may be the best way to specify this.

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define d
  #<<|
0 @I1@ INDI
1 NAME John Brinckerhoff /Clements/
2 GIVN John
2 _MIDN Brinckerhoff
2 SURN Clements
1 SEX M
1 BIRT
2 DATE 23 Aug 1971
2 PLAC Berkeley, Alameda, California, United States
1 EMAIL aoeuwikitree@brinckerhoff.org
|
  )

(define lines (regexp-split #px"\n" d))

