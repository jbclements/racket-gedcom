#lang racket

(require sugar)

;; this parser is totally incomplete.

;; this parser is based on "GEDCOM 5.5.1 AE Revision 2",
;; downloaded from https://www.gedcom.org/gedcom.html

;; it does successfully parse at least one GED file.

;; it uses racket regexps.

;; top-level notes on spec:
;; 1) it appears that underscore should be legal in any_char
;; 2) given the comments in the file, shouldn't \t be moved
;;    out of other_char and into any_char?

;; these could just be problems with FamilyTree's gedcom:
;; 1) The "DATE" tag appears with a trailing space but no value, which
;;    is technically illegal but not a big deal (e.g. "2 DATE ")
;; 2) as expected, at signs are used unquoted in many email addresses.
;; 3) tabs appear in many line_item's. Illegal, right?
;; 4) There's heavy use of pointers in free text, as in
;;    "2 CONT : Child:  @I123@". Looking at some of these, it looks like
;;    these are mostly quoting failures. Ouch.

;; level 2 notes, wikitree failures:
;; 1) tags like BIRT and DATE often appear 

(define error-bin (box '()))
(define (parse-error! line message)
  (set-box! error-bin (cons (list line message)
                            (unbox error-bin))))
(define (reset-parse-errors!)
  (set-box! error-bin '()))
(define (errors)
  (sort (unbox error-bin) < #:key first))

(define (list-with-indices l)
  (for/list ([i (in-naturals)]
             [elt (in-list l)])
    (list i elt)))

(define (pair-with-indices l)
  (for/list ([i (in-naturals)]
             [elt (in-list l)])
    (cons i elt)))

(define lines
  (time
   ;; more efficient to deforest? not worrying.
   (list-with-indices (file->lines "/tmp/foo2.ged"))))

(printf "this file contains ~v lines.\n"
        (length lines))

(define (blank-line? l)
  (regexp-match #px"^[[:space:]]*$" (second l)))

(let ()
  (define blank-line-count
    (count blank-line? lines))

  (when (not (equal? 0 blank-line-count))
    (printf "it contains ~v blank lines, which are apparently illegal.\n"
            blank-line-count)
    (printf "Let's just ... ignore them?\n")))

(define non-blank-lines
  (filter (compose not blank-line?) lines))

(define (starts-with-whitespace? l)
  (regexp-match #px"^[[:space:]]+" (second l)))

(let ()
  (define bad-lines
    (filter starts-with-whitespace? non-blank-lines))
  (define starts-with-whitespace-count
    (length bad-lines))
  
  (when (not (equal? 0 starts-with-whitespace-count))
    (printf "It contains ~v lines that start with whitespace, also illegal.\n"
            starts-with-whitespace-count)

    (printf "here are the first few (up to 10):\n")
    (pretty-print (take bad-lines (min 10 (length bad-lines))))
    (printf "\nsigh.\n\n")))

(printf "long lines: ~v\n"
        (count (λ (l) (< 255 (string-length (second l)))) non-blank-lines))

;; there's some serious breakage in and around these "Comment" objects.
;; Specifically, the exported gedcom files aren't quoting these lines the
;; way they pretty clearly should be. Ugh. They should be repaired with "CONT"
;; tags, I believe. Separate pass, bleah.

(define (repair-comments lines)
  (cond
    [(empty? lines)
     '()]
    [else
     (match lines
       [(list-rest (list _ (regexp #px"[0-9]{1,2} OBJE"))
                   (list _ (regexp #px"[0-9]{1,2} FORM Comment"))
                   (list _ (regexp #px"[0-9]{1,2} DATE"))
                   (list _ (regexp #px"([0-9]{1,2}) AUTH" (list _ levelstr)))
                   remainder)
        (append (take lines 4)
                (repair-comments/2a (string->number levelstr) "_BOGO" remainder))]
       [other
        (cons (first lines)
              (repair-comments (rest lines)))])]))

;; this line is a member of a block that probably needs levels & tags, sigh.
;; this is still fragile, you could easily inject a comment with a "0 INDI"
(define (repair-comments/2a level tag lines)
  (cond [(empty? lines) '()]
        [else
         (define line (first lines))
         (match (second line)
           [(regexp #px"^([0-9]{1,2}) (@[^@]+@ )?_?[A-Z]+( |$)" (list _ levelstr _ _))
            (cond [(< (string->number levelstr) level)
                   ;; looks like a real line, continue...
                   (repair-comments lines)]
                  [else
                   (error 'repair-comments "probably an error here on line ~v: ~e"
                          (first line)
                          (second line))])]
           [else
            (define next-level
              (cond [(equal? tag "CONT") level]
                    [else (add1 level)]))
            (cons
             (list (first line)
                   (~a level " " tag " " (second line)))
             (repair-comments/2a next-level "CONT" (rest lines)))])]))

(check-equal?
 (repair-comments
  (list-with-indices
   (regexp-split #px"\n"
                 #<<|
1 OBJE
2 FORM Comment
2 DATE 5 Dec 2017
2 AUTH Whitney Rapp
<a href="/wiki/Wright-17912" title="Wright-17912">Wright-17912</a> an
d Wright-23327 appear to represent the same person because: same person
0 @I58@ INDI
1 NAME Joseph  /Wright/
|
                 ))
  )
 '((0 "1 OBJE")
   (1 "2 FORM Comment")
   (2 "2 DATE 5 Dec 2017")
   (3 "2 AUTH Whitney Rapp")
   (4 "2 _BOGO <a href=\"/wiki/Wright-17912\" title=\"Wright-17912\">Wright-17912</a> an")
   (5 "3 CONT d Wright-23327 appear to represent the same person because: same person")
   (6 "0 @I58@ INDI")
   (7 "1 NAME Joseph  /Wright/")))

(define repaired-lines
  (time (repair-comments non-blank-lines)))

;; okay, we're going to be gluing together strings to make regular expressions.
;; this is a bad idea, and I'd rather use something like Olin Shivers' SREs, but
;; I can't currently find a working implementation.

;; also, the number of parens that are required for grouping of "or" blocks means
;; that the resulting regexp has a freakish number of submatches. Most of them
;; must be ignored. Ugh.

;; a nonzero digit:
(define nzdigit "[1-9]")

; delim:= (0x20)
;; a space
(define delim " ")

; digit:= [(0x30)-(0x39) ]
;; a standard digit

;level:=
;[ digit | digit + digit ]
;; one or two digits, with 2 digits only second can be zero (side condition stated in text)
(define level "([0-9]|[1-9][0-9])")


; non_at:=
; [ alpha | digit | otherchar | (0x23) | (0x20 ) ]

;; okay, this is a bit silly; the otherchar definition is contentious,
;; and it looks non_at reduces to "all printing characters other than @, #, and _
;; also, this "print" should almost certainly be extended to cover all
;; unicode printing chars.
(define non-at "[^@#_[:space:]]")


; pointer_char:= [ non_at ]
(define pointer-char non-at)

; pointer_string:=
; [ null | pointer_char | pointer_string + pointer_char ]

;; this seems like a long-winded way to write it... the second one is unnecessary?

(define pointer-string
  (~a pointer-char "*"))

; pointer:=
; (0x40) + alphanum + pointer_string + (0x40)
;; at-sign-wrapped string, must be a first char
;; submatched

(define pointer
  (~a "@([[:alnum:]]"pointer-string")@"))


; xref_ID:= pointer

(define xref-id pointer)

; optional_xref_ID:= xref_ID + delim
(define opt-xref-id (~a xref-id delim))



;tag:=
; [ [(0x5F)] + alphanum | tag + alphanum ]

(define tag
  "(_?[[:alnum:]]+)")


; any_char:=
; [ alpha | digit | otherchar | (0x23) | (0x20) | (0x40)+(0x40) ]

;; it really looks like this should have included underscore. Hmm..
;; okay, putting it in there for now. File  a bug report against the spec?
(define any-char
  (~a "([^@]|@@)"))

;escape_text:=
; [ any_char | escape_text + any_char ]

(define escape-text
  (~a "("any-char")+"))

; escape:=
; (0x40) + (0x23) + escape_text + (0x40)
(define escape
  (~a "@#("escape-text")@"))

;; line_text:= [ any_char | line_text + any_char ]
(define line-text
  escape-text)

; line_item:=
; [ escape | line_text | escape + delim + line_text ]

(define line-item
  ;; will this be matched efficiently?
  (~a "("escape delim line-text"|"escape"|"line-text")"))

;; this is important, to allow replacement of #f with "" later
;; in line parsing.
(check-false (regexp-match? line-item ""))


;line_value:=
;[ pointer | line_item ]

(define line-value
  (~a "("pointer"|"line-item")"))

(define optional-line-value
  (~a " " line-value))

;gedcom_line:=
;level + delim + [optional_xref_ID] + tag + [optional_line_value] + terminator
(define gedcom-line
  (pregexp (~a "^" level delim "("opt-xref-id")?" tag "("optional-line-value")?" "$")))

;; A line that doesn't match the previous pattern
;; useful to prevent parsing breakage.
(define exceptional-gedcom-line
  (pregexp (~a "^" level delim tag delim "(.*)$")))




;; should not fail to match....
(check-equal? (regexp-match gedcom-line "0 HEAD")
              '("0 HEAD"
                "0" ;; level
                #f ;; bogus
                #f ;; xref name
                "HEAD" ;; tag
                 #f ;; delim+line-value
                 #f ;; line-value
                 #f #f #f #f #f #f #f #f #f #f #f #f ;; oh dear heaven
                 ))
(check-equal? (take (regexp-match gedcom-line "1 SOUR WikiTree.com") 7)
             '("1 SOUR WikiTree.com"
               "1" #f #f
                   "SOUR"
                   " WikiTree.com"
                   "WikiTree.com"))

(check-equal? (take (regexp-match gedcom-line "2 TYPE wikitree.page_id") 7)
              '("2 TYPE wikitree.page_id"
                "2" #f #f
                "TYPE"
                " wikitree.page_id"
                "wikitree.page_id"))

(check-equal? (take (regexp-match gedcom-line "1 FAMS @F9@") 7)
              '("1 FAMS @F9@"
                "1" #f #f
                "FAMS"
                " @F9@"
                "@F9@"))
(regexp-match gedcom-line "1 FAMS @F9@")



;; the structure of the given grammar makes it hard to separate parsing
;; into a traditional tokenizer/parser format, so this is going to be
;; ad-hoc, like every other parser in the world, sigh.
;; gedcom_line ::= level + delim +  [optional_xref_ID] + tag + [optional_line_value] + terminator
;; returns
;; (U (List Natural 'totally-illegal-line String)
;;   (List Natural Natural (U String False) String String)
;;   (List Natural Natural (U String False) String 'partly-illegal-line String))
(define (parse-gedcom-line l)
  (define line-num (first l))
  (define line-content (second l))
  (match line-content
    [(regexp gedcom-line
             ;; the "list-rest" is necessary because the regexp
             ;; requires many extra pairs of parens for grouping,
             ;; are treated as extra match locations.
             (list-rest _ levelstr _ maybe-xref-id
                        tag _ line-text _))
     (list line-num ;; useful for error messages...
           (string->number levelstr)
           maybe-xref-id
           tag
           ;; this is not collapsing cases because line-text
           ;; doesn't match the empty string
           (or line-text ""))]
    ;; we get improved parsing and error reporting when we partially parse
    ;; these illegal lines.
    [(regexp exceptional-gedcom-line (list _ levelstr tag rest-of-line))
     (list line-num
                  (string->number levelstr)
                  #f
                  tag
                  'partly-illegal-line
                  rest-of-line)]
    [other (list (first l) 'totally-illegal-line (second l))]))

;; this one is totally legal:
(check-equal? (parse-gedcom-line (list 1234 "74 CONT"))
              '(1234 74 #f "CONT" ""))
;; this one is technically not:
(check-equal? (parse-gedcom-line (list 1234 "74 CONT "))
              '(1234 74 #f "CONT" partly-illegal-line ""))

(define parsed-lines
  (time
   (map parse-gedcom-line repaired-lines)))


(define (totally-illegal-line? l)
  (equal? (second l) 'totally-illegal-line))

(printf "totally illegal lines: ~v\n"
        (count totally-illegal-line? parsed-lines))

(define (illegal-line? l)
  (or (totally-illegal-line? l)
      (equal? (fifth l) 'partly-illegal-line)))

(printf "illegal lines: ~v\n"
        (count illegal-line? parsed-lines))


"a frequency hash of the most common illegal lines:"
(take
 (sort
  (hash->list
   (frequency-hash
    (map rest
         (filter illegal-line? parsed-lines))))
  >
  #:key cdr)
 15)


(define (line-number l) (first l))

(define (line-level l) (second l))

;; represent a gedcom record
(struct ged-record (line-num opt-ptr tag line-rest subrecords) #:transparent)
(struct bogus-line (line-num content))
;; add a bit of type checking
(define ptr? string?) ;; can we do better here?
(define opt-ptr? (or/c false? ptr?))
(define tag? string?)
(define subrecord? (or/c ged-record? bogus-line?))


(define/contract (make-ged-record line-num opt-ptr tag line-rest subrecords)
  (-> natural? opt-ptr? tag? string? (listof subrecord?) ged-record?)
  (ged-record line-num opt-ptr tag line-rest subrecords))

;; given the current level and a list of lines, return
;; a list containg a list of parsed lines and a list of unused lines
;; result: (listof ged-record)
(define (record-parser expected-level lines)
  (match lines
    ['() (list '() '())]
    [(cons f r)
     (cond [(totally-illegal-line? f)
            ;; for now, treat illegal lines as members of the subs list...
            ;; but they can't have subs themselves, of course.
            (match-define (list rest-these new-lines-2)
              (record-parser expected-level r))
            (list (cons (bogus-line (first f) (second f))
                        rest-these)
                  new-lines-2)
            ]
           [(< (line-level f) expected-level)
            ;; no consume, pop out
            (list '() lines)]
           [(> (line-level f) expected-level)
            ;; ouch, error
            (error 'jump-to-larger-num
                   "jump to larger num on line ~v: expected <= ~v, got ~v"
                   (line-number f)
                   expected-level
                   (line-level f))]
           [else
            ;; try to parse sub-records:
            (match-define (list subs new-lines)
              (record-parser (add1 expected-level) r))
            ;; parse the rest of the records at this level:
            (match-define (list rest-these new-lines-2)
              (record-parser expected-level new-lines))
            ;; at this point we're just going to treat partly-illegal
            ;; lines as legal lines, sigh.
            (define line-content
              (cond [(equal? (fifth f) 'partly-illegal-line)
                     (sixth f)]
                    [else
                     (fifth f)]))
            (list (cons (make-ged-record (line-number f)
                                         (third f) (fourth f)
                                         line-content subs)
                        rest-these)
                  new-lines-2)])]))

(define (parse-all-lines lines)
  (match (record-parser 0 lines)
    [(list records leftover)
     (cond [(not (empty? leftover))
            ;; should be unreachable; this would occur if a line started
            ;; with a negative number, but the pattern for lines won't
            ;; allow negative numbers.
            (error 'internal-error
                   "should be unreachable" )]
           [else records])]))

(define legally-empty '("CONT" "TRLR"))

;; validation: no record can have both a pointer and a value.
;; if a record has no subrecords, then it must have either a pointer or a value.
(define (check-record ged-rec)
  (match ged-rec
    [(bogus-line _ _)
     '()]
    [(ged-record _ opt-ptr tag value subrecords)
     (define this-line-errors
       (cond [(and (empty? subrecords)
                   (not opt-ptr)
                   (equal? value "")
                   (not (member tag legally-empty)))
              (list (list 'too-empty ged-rec))]
             [(and opt-ptr (not (equal? value "")))
              (list (list 'too-full ged-rec))]
             [else '()]))
     (define sub-errors
       (apply append (map check-record subrecords)))
     (append this-line-errors sub-errors)]))




(define example-data
  (pair-with-indices
'((0 "I2299" "INDI" "")
  (1 #f "NAME" " Jane  /Unknown/")
  (2 #f "GIVN" " Jane")
  (2 #f "_AKA" " Rhydderch")
  (1 #f "SEX" " F")
  (1 #f "BIRT" "")
  (2 #f "DATE" " ABT 1651")
  (2 #f "PLAC" " Alltgoch, Llanwenog, Cardiganshire, Wales")
  (1 #f "DEAT" "")
  (2 #f "DATE" " ABT 1710")
  (2 #f "PLAC" " Pembrokeshire, Wales")
  (1 #f "WWW" " https://www.WikiTree.com/wiki/Unknown-346770 ")
  (1 #f "FAMC" " @F581@")
  (1 #f "FAMS" " @F577@")
  (1 #f "NOTE" " {{Unsourced|Cardiganshire|Pembrokeshire}}")
  (2 #f "CONT" " == Biography ==")
  (2 #f "CONT" " ")
  (2
   #f
   "CONT"
   " :b. 1652, Alltgoch, Llanwenog, Cardiganshire, Wales{{Citation needed}}")
  (2 #f "CONT" " :d. 1681, Pembrokeshire, Wales{{Citation needed}}")
  (2 #f "CONT" " ")
  
  (2 #f "CONT" " ")
  (2 #f "CONT" " ")
  (2 #f "CONT" " == Sources ==")
  (2 #f "CONT" " <references />")
  (2 #f "CONT" " See also:")
  (2 #f "CONT" " ")
  (2
   #f
   "CONC"
   " * [[Wendt-138 | Greg Wendt]] created WikiTree profile De Ceredigion-1 ")
  (2 #f "CONC" " through the import of Wendt.ged on May 26, 2013. ")
  (2 #f "CONT" " ")
  (2
   #f
   "CONC"
   " * Created by [[Leopard-12 | William Leopard]] through the import of bu")
  (2 #f "CONC" " d rumbley.ged on Feb  3, 2014.")
  (1 #f "REFN" " 14665447")
  (2 #f "TYPE" " wikitree.user_id")
  (1 #f "REFN" " 15724753")
  (2 #f "TYPE" " wikitree.page_id")
  (1 #f "REFN" " 60")
  (2 #f "TYPE" " wikitree.privacy")
  (1 #f "OBJE" "")
  (2 #f "FORM" " Comment")
  (2 #f "DATE" " 9 Dec 2016")
  (2 #f "AUTH" " Jonathan Griffith")
  (totally-illegal-line
   "<a href=\"/wiki/De_Ceredigion-1\" title=\"De Ceredigion-1\">De Ceredigion-1</a>\
 and Rhydderch-17 appear to represent the same person because: Same first name, married\
 to the same two people, and birth/death dates are close enough to match.")
  (1 #f "OBJE" "")
  (2 #f "FORM" " Comment")
  (2 #f "DATE" " 30 Oct 2015")
  (2 #f "AUTH" " Julie Ricketts")
  (totally-illegal-line
   "<a href=\"/wiki/UNKNOWN-227477\" title=\"UNKNOWN-227477\">UNKNOWN-227477</a>\
 and De Ceredigion-1 appear to represent the same person because: Given the dates,\
 these appear to represent the same person.")
  (1 #f "OBJE" "")
  (2 #f "FORM" " Comment")
  (2 #f "DATE" " 3 Jan 2015")
  (2 #f "AUTH" " Cari Lynn G")
  (totally-illegal-line
   "<a href=\"/wiki/De_Ceredigion-1\" title=\"De Ceredigion-1\">De Ceredigion-1</a>\
 and Rhydderch-16 appear to represent the same person because: I think that these are\
 the same women."))))

(define example-data2
  (pair-with-indices
'((0 "I2299" "INDI" "")
  (1 #f "NAME" " Jane  /Unknown/")
  (2 #f "_AKA" " Rhydderch")
  (1 #f "BIRT" "")
  (2 #f "DATE" " ABT 1651")
  (2 #f "PLAC" " Alltgoch, Llanwenog, Cardiganshire, Wales")
  (1 #f "DEAT" "")
  (0 "I3299" "INDI" "")
  (1 #f "NAME" " Jane  /UnknoXwn/")
  (2 #f "_AKA" " RhyddeXrch")
  (1 #f "BIRT" "")
  (2 #f "DATE" " ABBT 1651")
  (2 #f "PLAC" " Allgoch, Llanwenog, Cardiganshire, Wales")
  (1 #f "DEAT" "")
  )))

(define example-data2-result
  (list
   (ged-record
    0
    "I2299" "INDI" ""
    (list
     (ged-record
      1
      #f "NAME" " Jane  /Unknown/"
      (list (ged-record 2 #f "_AKA" " Rhydderch" '())))
     (ged-record
      3
      #f "BIRT" ""
      (list (ged-record 4 #f "DATE" " ABT 1651" '())
            (ged-record 5 #f "PLAC" " Alltgoch, Llanwenog, Cardiganshire, Wales" '())))
     (ged-record 6 #f "DEAT" "" '())))
   (ged-record 7
    "I3299" "INDI" ""
    (list
     (ged-record 8
      #f "NAME" " Jane  /UnknoXwn/"
      (list (ged-record 9 #f #"_AKA" " RhyddeXrch" '())))
     (ged-record 10
      #f "BIRT" ""
      (list (ged-record 11 #f "DATE" " ABT 1651" '())
            (ged-record 12 #f "PLAC" " Alltgoch, Llanwenog, Cardiganshire, Wales" '())))
     (ged-record 13 #f "DEAT" "" '())))))

(check-equal? (first (parse-all-lines example-data2))
              (first example-data2-result))




(require rackunit)

(check-not-exn (λ () (record-parser 0 example-data)))


(define d (time
           (parse-all-lines parsed-lines)))

(define errors2 (apply append (map check-record d)))

(printf "these are all believed to be wikitree gedcom bugs:\n")
(printf "frequency of check-record errors:\n")
(frequency-hash (map first errors2))
(printf "frequency of too-empty tags:\n")
(frequency-hash (map ged-record-tag
                     (map second
                          (filter (λ (e) (equal? (first e) 'too-empty)) errors2))))

(printf "file contains ~v top-level records\n"
        (length d))


