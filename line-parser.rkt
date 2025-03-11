#lang racket

(require rackunit
         sugar
         scramble/regexp)

;; this file parses the lines into parsed-lines. Makes sure the lines
;; are legal, produce a structured output. This is the stage that cares
;; about regexps and the line-level gedcom grammar.

(provide (contract-out
          [parse-lines
           (-> (listof (list/c nat? string?)) (listof parsed-line?))]
          [parse-gedcom-line
           (-> (list/c nat? string?) parsed-line?)])
         legal-line?
         partly-illegal-line?
         totally-illegal-line?)

(define nat? exact-nonnegative-integer?)


(define line-content?
  (or/c string?
        (list/c 'pointer string?)
        (list/c 'escape string?)
        (list/c 'escape-and-str string? string?)))

;; a legal-line contains
;; line-number, depth, maybe-ptr, maybe-tag, content
(define legal-line?
   (list/c nat? nat? (or/c string? false?) string? line-content?))

(define partly-illegal-line?
  ;; line-number, depth, maybe-ptr, maybe-tag,
  (list/c nat? nat? (or/c string? false?) string? 'partly-illegal-line
          string?))

(define totally-illegal-line?
  (list/c nat? 'totally-illegal-line string?))


(define parsed-line?
  (or/c
   totally-illegal-line?
   partly-illegal-line?
   legal-line?))



;; is this line partly or totally illegal?
(define (illegal-line? l)
  (or (totally-illegal-line? l)
      (partly-illegal-line? l)))

;; stage 3: parse the lines.
;; EFFECT: report illegal lines
(define (parse-lines repaired-lines)
  (printf "LINE PARSING...\n")
  (define parsed-lines
    (time (map parse-gedcom-line repaired-lines)))

  (printf "totally illegal lines: ~v\n"
          (count totally-illegal-line? parsed-lines))

  (printf "partly illegal lines: ~v\n"
          (count partly-illegal-line? parsed-lines))

  "a frequency hash of the most common illegal lines:"
  (let ()

    (define illegal-lines
      (sort
       (hash->list
        (frequency-hash
         (map rest
              (filter illegal-line? parsed-lines))))
       >
       #:key cdr))
    (take
     illegal-lines
     (min 15 (length illegal-lines))))

  parsed-lines)

;; input: (List Natural String)
;; output:
;; (U (List Natural 'totally-illegal-line String)
;;   (List Natural Natural (U String False) String String)
;;   (List Natural Natural (U String False) String 'partly-illegal-line String))

;; the structure of the given grammar makes it hard to separate parsing
;; into a traditional tokenizer/parser format, so this is going to be
;; ad-hoc, like every other parser in the world, sigh.
;; gedcom_line ::= level + delim +  [optional_xref_ID] + tag + [optional_line_value] + terminator
(define (parse-gedcom-line l)
  (define line-num (first l))
  (define line-content (second l))
  (match line-content
    [(regexp gedcom-line
             ;; the "list-rest" is necessary because the regexp
             ;; requires many extra pairs of parens for grouping,
             ;; are treated as extra match locations.
             (list-rest _ levelstr maybe-xref-id
                        tag line-text _))
     ;; this is not collapsing cases because line-text
     ;; doesn't match the empty string
     (define line-value
       (cond [line-text
              ;; can't be empty by definition of regexp
              ;; this test should be fast, to make the common case fast:
              (cond [(equal? (string-ref line-text 0) #\@)
                     (cond [(regexp-match pointer-rx line-text)
                            (list 'pointer (substring line-text
                                                      1 (sub1 (string-length line-text))))]
                           [(regexp-match? escape-rx line-text)
                            ;;; sigh....
                            (match line-text
                              [(regexp escape-beginning-line-item-rx
                                       ;; unbelievably sensitive... (getting better)
                                       (list _ #f #f str))
                               (list 'escape str)]
                              [(regexp escape-beginning-line-item-rx
                                       (list _ str rest #f))
                               (list 'escape-and-str str rest)])]
                           [else
                            (error 'parse-gedcom-line
                                   "internal error, this should be unreachable 20211124")])]
                    [else line-text])]
             [else ""]))
     (list line-num ;; useful for error messages...
           (string->number levelstr)
           maybe-xref-id
           tag
           line-value)]
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

;; TOP LEVEL COMMENT: REGEXPS ARE TERRIBLE.

;; more specifically: they're not a terrible way to try to specify a set of inputs,
;; but they're a terrible terrible way to build a parser. They are incredibly hard
;; to reason about, and the parser below contains all kinds of hidden freaky dependencies.
;; part of that (a huge part) is the fact that the regexp matcher just produces a
;; match, not a parse tree, and decorating that with a huge number of parens just creates
;; a terrifying flat list. Beyond that, though, there are all kinds of possible ambiguities;
;; nothing checks a regexp to make sure that it doesn't have multiple parse trees. Ugh.

;; okay, we're going to be gluing together strings to make regular expressions.
;; this is a bad idea, and I'd rather use something like Olin Shivers' SREs, but
;; I can't currently find a working implementation. ... Note: looks like irregex
;; does this, and I should probably change to use irregex. My gross string-gluing
;; seems to be working for now (2021-11-24)

;; also, the number of parens that are required for grouping of "or" blocks means
;; that the resulting regexp has a freakish number of submatches. Most of them
;; must be ignored. Ugh.

;; 2025-03-09 This has all gotten way way better with Ryan Culpepper's scramble. Yay!

;; a nonzero digit:
(define-RE nzdigit (chars [#\1 #\9]))

; delim:= (0x20)
;; a space
(define-RE delim " ")

; digit:= [(0x30)-(0x39) ]
;; a standard digit

;level:=
;[ digit | digit + digit ]
;; one or two digits, with 2 digits only second can be zero (side condition stated in text)
(define-RE level (or (chars digit) (cat nzdigit (chars digit))))


; non_at:=
; [ alpha | digit | otherchar | (0x23) | (0x20 ) ]

;; okay, this is a bit silly; the otherchar definition is contentious,
;; and it looks non_at reduces to "all printing characters other than @, #, and _
;; also, this "print" should almost certainly be extended to cover all
;; unicode printing chars.
(define-RE non-at (chars (complement "@#_" space)))

; pointer_char:= [ non_at ]
(define-RE pointer-char non-at)

; pointer_string:=
; [ null | pointer_char | pointer_string + pointer_char ]

;; this seems like a long-winded way to write it... the second one is unnecessary?
(define-RE pointer-string (repeat pointer-char))

; pointer:=
; (0x40) + alphanum + pointer_string + (0x40)
;; at-sign-wrapped string, must be a first char
;; submatched
(define-RE pointer
  (cat "@" (report (cat (chars alnum) pointer-string)) "@"))


; xref_ID:= pointer
(define-RE xref-id pointer)

; optional_xref_ID:= xref_ID + delim
(define-RE opt-xref-id (cat xref-id delim))

;tag:=
; [ [(0x5F)] + alphanum | tag + alphanum ]
(define-RE tag (cat (? "_") (+ (chars alnum))))

; any_char:=
; [ alpha | digit | otherchar | (0x23) | (0x20) | (0x40)+(0x40) ]

;; it really looks like this should have included underscore. Hmm..
;; okay, putting it in there for now. File  a bug report against the spec?
(define-RE any-char
  (or (chars (complement "@"))
      "@@"))

;escape_text:=
; [ any_char | escape_text + any_char ]
(define-RE escape-text (+ any-char))

;; NB: the term "escape" here is used in a very different sense that I would expect.
;; specifically, it specifically allows an "extra tag" to be used to specify a
;; calendar, with an optional line_item string following it. Weird.
; escape:=
; (0x40) + (0x23) + escape_text + (0x40)
(define-RE escape
  (cat "@#" (report escape-text) "@"))

;; line_text:= [ any_char | line_text + any_char ]
(define-RE line-text escape-text)

; line_item:=
; [ escape | line_text | escape + delim + line_text ]

(define-RE line-item
  (or (cat escape delim line-text)
      escape
      line-text))
;; this is used to parse line-items known to begin with an escape
(define-RE escape-beginning-line-item
  (or (cat escape delim (report line-text))
      escape))

;; this is important, to allow replacement of #f with "" later
;; in line parsing.
(check-false (regexp-match? (px line-item) ""))

;line_value:=
;[ pointer | line_item ]
(define-RE line-value (or pointer line-item))

;; regexp wrappers

(define escape-beginning-line-item-rx (px ^ escape-beginning-line-item))
(define pointer-rx (px ^ pointer))
(define escape-rx (px ^ escape))


;gedcom_line:=
;level + delim + [optional_xref_ID] + tag + [optional_line_value] + terminator
(define gedcom-line
  (px ^ (report level)
      delim
      (? opt-xref-id)
      (report tag)
      (? (cat delim (report line-value)))
      $))

;; A catch-all line that doesn't match the previous pattern
;; useful to prevent parsing breakage.
(define exceptional-gedcom-line
  (px ^ (report level) delim (report tag) delim (report (* (inject "."))) $))


;; should not fail to match....
(check-equal? (regexp-match gedcom-line "0 HEAD")
              '("0 HEAD"
                "0" ;; level
                #f ;; xref name
                "HEAD" ;; tag
                 #f ;; line-value
                 #f #f #f ;; just for grouping...
                 ))
(check-equal? (take (regexp-match gedcom-line "1 SOUR WikiTree.com") 5)
             '("1 SOUR WikiTree.com"
               "1" #f
                   "SOUR"
                   "WikiTree.com"))

(check-equal? (take (regexp-match gedcom-line "2 TYPE wikitree.page_id") 5)
              '("2 TYPE wikitree.page_id"
                "2" #f
                "TYPE"
                "wikitree.page_id"))

(check-equal? (take (regexp-match gedcom-line "1 FAMS @F9@") 5)
              '("1 FAMS @F9@"
                "1" #f
                "FAMS"
                "@F9@"))


(check-equal? (parse-gedcom-line (list 3 "4 ABC foo bar baz quux"))
              '(3 4 #f "ABC" "foo bar baz quux"))

(check-equal? (parse-gedcom-line (list 3 "4 ABC @ghijkl@"))
              '(3 4 #f "ABC" (pointer "ghijkl")))

(check-equal? (parse-gedcom-line (list 3 "4 ABC @#ghijkl@"))
              '(3 4 #f "ABC" (escape "ghijkl")))

(check-equal? (parse-gedcom-line (list 3 "4 ABC @#ghijkl@ nana"))
              '(3 4 #f "ABC" (escape-and-str "ghijkl" "nana")))

;; this one is totally legal:
(check-equal? (parse-gedcom-line (list 1234 "74 CONT"))
              '(1234 74 #f "CONT" ""))
;; this one is technically not:
(check-equal? (parse-gedcom-line (list 1234 "74 CONT "))
              '(1234 74 #f "CONT" partly-illegal-line ""))