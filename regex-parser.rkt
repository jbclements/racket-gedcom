#lang racket

;; okay, I'm up to two serious bugs in irregex, I'm giving up on it for now.

(require irregex)

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

;; a nonzero digit:
(define nzdigit '(: (/ #\1 #\9)))

;delim:= (0x20)

(define delim '(: #\space))

; digit:=
; [(0x30)-(0x39) ]
(define digit '(/ #\0 #\9))

;level:=
;[ digit | digit + digit ]

(define level `(=> level (or ,nzdigit (: ,nzdigit ,digit))))



; non_at:=
; [ alpha | digit | otherchar | (0x23) | (0x20 ) ]

;; okay, this is a bit silly; the otherchar definition is contentious,
;; and it looks non_at reduces to "all printing characters other than @ and _
;; also, this "print" should almost certainly be extended to cover all
;; unicode printing chars.

(define non-at
  `(- print ("@_")))

; pointer_char:= [ non_at ]

(define pointer-char non-at)

; pointer_string:=
; [ null | pointer_char | pointer_string + pointer_char ]

;; this seems like a long-winded way to write it... the second one is unnecessary?

(define pointer-string
  `(* ,pointer-char))

; pointer:=
; (0x40) + alphanum + pointer_string + (0x40)

(define pointer
  `(: #\@ alphanum ,pointer-string #\@))


; xref_ID:= pointer

(define xref-id `(=> xref-id ,pointer))



;tag:=
; [ [(0x5F)] + alphanum | tag + alphanum ]

(define tag
  `(: (? ("_")) (+ alphanum)))

; escape:=
; (0x40) + (0x23) + escape_text + (0x40)
(define escape)

; line_item:=
; [ escape | line_text | escape + delim + line_text ]

(define line-item
  ;; will this be matched efficiently?
  `(or ,escape ,line-text (: ,escape ,delim ,line-text)))


;line_value:=
;[ pointer | line_item ]

(define line-value
  `(or ,pointer ,line-item))

;gedcom_line:=
;level + delim + [optional_xref_ID] + tag + [optional_line_value] + terminator
(define gedcom-line
  `(: ,level ,delim (? ,xref-id) ,tag (? ,line-value)))



#| this looks like a bug:


|#

;(irregex-search '(or (:) "was") "abc")
#|vector-ref: index is out of range
  index: 3
  valid range: [0, 1]
  vector: '#(0 0)|#

#;(

   )

#;(



(irregex-search (irregex '(seq "match" (? "es") "!")) "matches!")

(irregex-search '(: "1234") "a1234" )

(irregex-search '(: "one" space "two" space "three") "a one two three")

(irregex-match-names
 (irregex-match '(: (=> level (** 1 2 num)) (* any)) "12341"))

(irregex-match-substring
 (irregex-match '(: (=> level (** 1 2 num)) (* any)) "12341")
 'level))
