#lang racket

(require sugar
         rackunit
         "shared.rkt"
         "line-parser.rkt")

(provide parse-gedcom)

;; I am not at all certain that this parser is complete.

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


;; looks like the error-bin mechanism is currently unused....
#;((define error-bin (box '()))
(define (parse-error! line message)
  (set-box! error-bin (cons (list line message)
                            (unbox error-bin))))
(define (reset-parse-errors!)
  (set-box! error-bin '()))
(define (errors)
  (sort (unbox error-bin) < #:key first)))

;; given a list '(a b c) return ((0 a) (1 b) (2 c))
(define (list-with-indices l)
  (for/list ([i (in-naturals)]
             [elt (in-list l)])
    (list i elt)))

;; given a list '(a b c) return ((0 . a) (1 . b) (2 . c)
(define (pair-with-indices l)
  (for/list ([i (in-naturals)]
             [elt (in-list l)])
    (cons i elt)))

;; is does this line consist only of whitespace?
(define (blank-line? l)
  (regexp-match #px"^[[:space:]]*$" (second l)))

;; does this line start with at least one whitespace character?
(define (starts-with-whitespace? l)
  (regexp-match #px"^[[:space:]]+" (second l)))

;; STAGE 1: 
;; given a file name, return a list of the non-blank lines
;; EFFECT: output about file contents.
(define (file-non-blank-lines file)
  (define lines
    (time
     ;; more efficient to deforest? not worrying.
     (list-with-indices (file->lines file))))

  (printf "this file contains ~v lines.\n"
          (length lines))

  (let ()
    (define blank-line-count
      (count blank-line? lines))

    (when (not (equal? 0 blank-line-count))
      (printf "it contains ~v blank lines, which are apparently illegal.\n"
              blank-line-count)
      (printf "Let's just ... ignore them?\n")))

  (define non-blank-lines
    (filter (compose not blank-line?) lines))

  non-blank-lines)

;; STAGE 2: repair broken comment lines

;; given a list of non-blank lines, repair comments and return a list of the lines
;; EFFECT: verbose reporting
(define (repair non-blank-lines)
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

  (repair-comments non-blank-lines))

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



(define (line-number l) (first l))

(define (line-level l) (second l))

;; represent a gedcom record
(struct ged-record (line-num opt-xref-id tag line-rest subrecords) #:transparent)
(struct bogus-line (line-num content))
;; add a bit of type checking
(define ptr? string?) ;; can we do better here?
(define opt-ptr? (or/c false? ptr?))
(define tag? string?)
(define subrecord? (or/c ged-record? bogus-line?))


(define/contract (make-ged-record line-num opt-ptr tag line-rest subrecords)
  (-> natural? opt-ptr? tag? (or/c string? (list/c 'pointer string?))
      (listof subrecord?) ged-record?)
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

(define (record->pointer-targets record)
  (define top-level-targets
    (cond [(ged-record-opt-xref-id record)
           =>
           (λ (x) (list x))]
          [else
           '()]))
  (apply append
         (cons top-level-targets
               (map record->pointer-targets (ged-record-subrecords record)))))

(check-equal? (record->pointer-targets
               (ged-record 7
                            "I3299" "INDI" ""
                            (list
                             (ged-record 8
                                         #f "NAME" " Jane  /UnknoXwn/"
                                         (list (ged-record 9 #f #"_AKA" " RhyddeXrch" '())))
                             (ged-record 10
                                         "ZZBAGGER" "BIRT" ""
                                         (list (ged-record 11 #f "DATE" " ABT 1651" '())
                                               (ged-record
                                                12 #f "PLAC"
                                                " Alltgoch, Llanwenog, Cardiganshire, Wales"
                                                '())))
                             (ged-record 13 #f "DEAT" "" '()))))
              '("I3299" "ZZBAGGER"))

(define (pointer-integrity-check records)
  (define all-pointer-targets
    (apply append (map record->pointer-targets records)))
  (let search ([records records])
    (for-each
     (λ (record)
       (match (ged-record-line-rest record)
         [(list 'pointer str)
          (when (not (member str all-pointer-targets))
            (error 'pointer-integrity-search
                   "pointer ~a has no matching target in these records"
                   str))]
         [other 'okay])
       (search (ged-record-subrecords record)))
     records)))

(check-not-exn
 (λ ()
   (pointer-integrity-check
    (list
     (ged-record 7
                 "I32199" "INDI" ""
                 (list
                  (ged-record 8
                              #f "NAME" " Jane  /UnknoXwn/"
                              (list (ged-record 9 #f #"_AKA" " RhyddeXrch" '())))
                  (ged-record 10
                              "ZZBAGGER" "BIRT" ""
                              (list (ged-record 11 #f "DATE" '(pointer "I32990") '())
                                    (ged-record 12 #f "PLAC"
                                                " Alltgoch, Llanwenog, Cardiganshire, Wales" '())))
                  (ged-record 13 #f "DEAT" "" '())))
     (ged-record 7
                 "I32990" "INDI" ""
                 (list
                  (ged-record 8
                              #f "NAME" " Jane  /UnknoXwn/"
                              (list (ged-record 9 #f '(pointer "I32199") " RhyddeXrch" '())))
                  (ged-record 10
                              "ZZBAGGER" "BIRT" ""
                              (list (ged-record 11 #f "DATE" " ABT 1651" '())
                                    (ged-record 12 #f "PLAC"
                                                " Alltgoch, Llanwenog, Cardiganshire, Wales" '())))
                  (ged-record 13 #f "DEAT" "" '())))))))

(check-exn
 #px"has no matching target"
 (λ ()
   (pointer-integrity-check
    (list
     (ged-record 7
                 "I32199" "INDI" ""
                 (list
                  (ged-record 8
                              #f "NAME" " Jane  /UnknoXwn/"
                              (list (ged-record 9 #f #"_AKA" " RhyddeXrch" '())))
                  (ged-record 10
                              "ZZBAGGER" "BIRT" ""
                              (list (ged-record 11 #f "DATE" '(pointer "I32990X") '())
                                    (ged-record 12 #f "PLAC"
                                                " Alltgoch, Llanwenog, Cardiganshire, Wales" '())))
                  (ged-record 13 #f "DEAT" "" '())))
     (ged-record 7
                 "I32990" "INDI" ""
                 (list
                  (ged-record 8
                              #f "NAME" " Jane  /UnknoXwn/"
                              (list (ged-record 9 #f '(pointer "I32199") " RhyddeXrch" '())))
                  (ged-record 10
                              "ZZBAGGER" "BIRT" ""
                              (list (ged-record 11 #f "DATE" " ABT 1651" '())
                                    (ged-record 12 #f "PLAC"
                                                " Alltgoch, Llanwenog, Cardiganshire, Wales" '())))
                  (ged-record 13 #f "DEAT" "" '())))))))

;; next, time to check the records themselves.


;; first record must be a HEAD





;; given a list of records, return a ged-hash
(define (records->hash records)
  ;; after thinking about this harder, it appears that there are a bunch
  ;; of "this before that" distinctions that just aren't important for this
  ;; data. It would appear to me that it preserves correctness to simply group
  ;; the user records by tag (preserving order, of course), and then pull the
  ;; tags out one at a time. It will of course be important to check that none
  ;; get left behind.
  ;; a map from tag to the records with that tag
  (define records-by-tag (group-by ged-record-tag records))
  (define prs (map tag-group-parse records-by-tag))
  (make-immutable-hash prs))

;; take the records with the same tag as the spec, parse them all, return
;; (cons tag treenodes) and list of errors
(define (tag-group-parse tag-group)
  (define records-and-errs
    (map record-parse tag-group))
  (cons (ged-record-tag (first tag-group)) records-and-errs))

(define (record-parse record)
  (treenode (ged-record-opt-xref-id record)
            (ged-record-line-rest record)
            (records->hash (ged-record-subrecords record))
            (ged-record-line-num record)))

;; Validation

;; a structure-pat contains a tag, a count specifier, an item spec and a list of structure-pats
;; OR just 'ANY

(define address-subpats 'ANY)

(define head-spec
  `(HEAD one empty
         ((SOUR one str
                ((VERS opt1 str ())
                 (NAME opt1 str ())
                 (CORP opt1 str
                       ,address-subpats)
                 (DATA opt1 str
                       ((DATE opt1 str ())
                        ;; CONT/CONC gone already?
                        (COPR opt1 str ())))))
          (DEST opt1 str ())
          (DATE opt1 str
                ((TIME opt1 str ())))
          (SUBM one ptr ())
          (FILE opt1 str ())
          (COPR opt1 str ())
          (GEDC one empty
                ((VERS one str ())
                 (FORM one str ())))
          (CHAR one str
                ((VERS opt1 str ())))
          (LANG opt1 str ())
          (PLAC opt1 empty
                ((FORM one str ())))
          (NOTE opt1 str ()))))

;; given a list of specs and a ged-hash, return a list of errors
(define (records-validate specs the-hash parent-line-num)
  (define spec-tags (map symbol->string (map first specs)))
  ;; since each tag corresponds to a hash key and/or a "field"
  ;; in a conceptual "structure",
  ;; it would be bad if a tag occurred more than once in a list of specs...
  (when (check-duplicates spec-tags)
    (error 'record-parse
           "duplicated tag name in list of specs: ~e"))
  (define record-tags (hash-keys the-hash))
  (define extra-tag-errs
    (let ()
      (define leftover-tags (set-subtract record-tags spec-tags))
      (cond [(not (empty? leftover-tags))
             ;; the line number of the first record with a tag that doesn't appear in the spec:
             (define first-line-num
               (first
                (sort
                 (map treenode-line-num
                      (apply append
                             (map (λ (tag) (hash-ref the-hash tag)) leftover-tags)))
                 <)))
             (list (list first-line-num
                         (format "tags not mentioned in the spec: ~e"
                                 leftover-tags)))]
            [else '()])))
  (define sub-errs (map (1spec-validate the-hash parent-line-num) specs))
  (apply append (cons extra-tag-errs sub-errs)))


;; take the records with the same tag as the spec, parse them all, return
;; list of errors
(define ((1spec-validate the-hash parent-line-num) spec)
  #;(printf "debug: spec: ~a\n" spec)
  (match-define (list tag requirement line-spec sub-specs) spec)
  (define matching-records (hash-ref the-hash (symbol->string tag) '()))
  #;(printf "debug: records\n~a\n"
          matching-records)
  (define count-errs
    (match (list requirement (length matching-records))
      [(list 'one 0) (list (list parent-line-num (format "missing required record ~e" tag)))]
      [(list 'one 1) '()]
      [(list 'one n) (list (list (treenode-line-num (first matching-records))
                                (format "more than one record with tag ~e" tag)))]
      [(list 'opt1 0) '()]
      [(list 'opt1 1) '()]
      [(list 'opt1 n) (list (list (treenode-line-num (first matching-records))
                                 (format "more than one record with tag ~e" tag)))]
      ;; there will be others here...
      ))
  (define sub-errs
    (match sub-specs
      ['ANY '()]
      [(list spec ...)
       (map (record-validate line-spec sub-specs)
         matching-records)]))
  (apply append (cons count-errs sub-errs)))


(define (non-empty-string? str)
  (not (equal? str "")))

(define (empty-string? str)
  (equal? str ""))

(define (pointer? l)
  (and (list? l)
       (not (empty? l))
       (equal? (first l) 'pointer)))

;; given a line-spec, a list of sub-specs, and a record,
;; return a list of errors
(define ((record-validate line-spec sub-specs) the-treenode)
  (match-define (treenode opt-xref-id line-content elements line-num)
    the-treenode)
  ;; we know it has the right tag.
  (define line-content-pred
    (match line-spec
      ['str non-empty-string?]
      ['empty empty-string?]
      ['ptr pointer?]))
  (define errs
    (cond [(line-content-pred line-content)
           '()]
          [else
           (list (list line-num
                       "expected string matching spec ~a, got: ~e"
                       line-spec
                       line-content))])
    )
  (match-define sub-errs
    (records-validate sub-specs elements line-num)) 
  (append errs sub-errs))



               

;; just to simplify my test cases...
(define (tnstr str)
  (tn str hash))
(define (tn str hash)
  (treenode #f str hash))
(define (tnhash hash)
  (treenode #f "" hash))

(define my-head
  (ged-record
 0
 #f
 "HEAD"
 ""
 (list
  (ged-record
   1
   #f
   "SOUR"
   "WikiTree.com"
   (list
    (ged-record 2 #f "NAME" "WikiTree: The Free Family Tree" '())
    (ged-record 3 #f "CORP" "Interesting.com, Inc." '())))
  (ged-record
   4
   #f
   "DATE"
   "31 May 2021"
   (list (ged-record 5 #f "TIME" "05:40:34 UTC" '())))
  (ged-record 6 #f "CHAR" "UTF-8" '())
  (ged-record 7 #f "FILE" "26051277-c85e6d.ged" '())
  (ged-record
   8
   #f
   "COPR"
   "Interesting.com, Inc. and John Clements"
   '())
  (ged-record 9 #f "SUBM" '(pointer "SUBM") '())
  (ged-record
   10
   #f
   "GEDC"
   ""
   (list
    (ged-record 11 #f "VERS" "5.5.1" '())
    (ged-record 12 #f "FORM" "LINEAGE-LINKED" '())))
  (ged-record
   13
   #f
   "NOTE"
   "This file contains private information and\
 may not be redistributed, published, or made public."
   '()))))





(check-equal?
 ((record-validate (third head-spec)
                   (fourth head-spec))
  (first (hash-ref (records->hash (list my-head)) "HEAD")))
 '())




(check-not-exn (λ () (record-parser 0 example-data)))

;; should get rid of CONC and CONT here?

;; LIVE:

(define (parse-gedcom gedcom-file)
  (define non-blank-lines (file-non-blank-lines gedcom-file))

  (define repaired-lines (time (repair non-blank-lines)))

  (define parsed-lines (parse-lines repaired-lines))

  (printf "RECORD PARSING...\n")
  (define d (time (parse-all-lines parsed-lines)))

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

  (pointer-integrity-check d)

  (records->hash d))
