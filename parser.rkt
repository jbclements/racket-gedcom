#lang racket

(require sugar
         parsack)

;; this parser is totally incomplete.

;; this parser is based on "GEDCOM 5.5.1 AE Revision 2",
;; downloaded from https://www.gedcom.org/gedcom.html

;; it does successfully parse at least one GED file.

(define error-bin (box '()))
(define (parse-error! line message)
  (set-box! error-bin (cons (list line message)
                            (unbox error-bin))))
(define (reset-parse-errors!)
  (set-box! error-bin '()))
(define (errors)
  (sort (unbox error-bin) < #:key first))

;;; hmmm... stopping here now.

(define lines
  (time
   (file->lines "/tmp/foo.ged")))

(printf "this file contains ~v lines.\n"
        (length lines))

(define (blank-line? l)
  (regexp-match #px"^[[:space:]]*$" l))

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
  (regexp-match #px"^[[:space:]]+" l))

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
        (count (λ (s) (< 255 (string-length s))) non-blank-lines))

;; this is not a careful implementation of the grammar specified in the


;; the structure of the given grammar makes it hard to separate parsing
;; into a traditional tokenizer/parser format, so this is going to be
;; ad-hoc, like every other parser in the world, sigh.
;; gedcom_line ::= level + delim +  [optional_xref_ID] + tag + [optional_line_value] + terminator
(define (parse-gedcom-line l)
  (match l
    [(regexp #px"^([1-9][0-9]?) (.*)$" (list _ nstr rest))
     (match rest
       [(regexp #px"^@([^@]+)@ (.*)$" (list _ ptr rest))
        (parse-post-ptr nstr ptr rest)]
       [other
        (parse-post-ptr nstr #f rest)])]
    [other (list 'illegal-line l)]))

(define (parse-post-ptr num maybe-ptr rest)
  (match rest
    [(regexp #px"^(_?[a-zA-Z0-9]+)(.*)" (list _ tag rest2))
     (list (string->number num) maybe-ptr tag rest2)]
    [other
     (error 'ouch "bad line continuation: ~e"
            rest)]))

(define parsed-lines
  (time (map parse-gedcom-line non-blank-lines)))


(define (illegal-line? l)
  (equal? (first l) 'illegal-line))


(define (line-level l)
  (first l))

;; represent a gedcom record
(struct ged-record (opt-ptr tag line-rest subrecords) #:transparent)

;; given the current level and a list of lines, return
;; a list containg a list of parsed lines and a list of unused lines
;; result: (listof ged-record)
(define (record-parser expected-level lines)
  (match lines
    ['() (list '() '())]
    [(cons f r)
     (cond [(illegal-line? f)
            ;; for now, treat illegal lines as members of the subs list...
            ;; but they can't have subs themselves, of course.
            (match-define (list rest-these new-lines-2)
              (record-parser expected-level r))
            (list (cons f rest-these)
                  new-lines-2)
            ]
           [(< (line-level f) expected-level)
            ;; no consume, pop out
            (list '() lines)]
           [(> (line-level f) expected-level)
            ;; ouch, error
            (error 'jump-to-larger-num)]
           [else
            ;; try to parse sub-records:
            (match-define (list subs new-lines)
              (record-parser (add1 expected-level) r))
            ;; parse the rest of the records at this level:
            (match-define (list rest-these new-lines-2)
              (record-parser expected-level new-lines))
            (list (cons (ged-record (second f) (third f) (fourth f) subs)
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

(define example-data
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
  (illegal-line
   "<a href=\"/wiki/De_Ceredigion-1\" title=\"De Ceredigion-1\">De Ceredigion-1</a>\
 and Rhydderch-17 appear to represent the same person because: Same first name, married\
 to the same two people, and birth/death dates are close enough to match.")
  (1 #f "OBJE" "")
  (2 #f "FORM" " Comment")
  (2 #f "DATE" " 30 Oct 2015")
  (2 #f "AUTH" " Julie Ricketts")
  (illegal-line
   "<a href=\"/wiki/UNKNOWN-227477\" title=\"UNKNOWN-227477\">UNKNOWN-227477</a>\
 and De Ceredigion-1 appear to represent the same person because: Given the dates,\
 these appear to represent the same person.")
  (1 #f "OBJE" "")
  (2 #f "FORM" " Comment")
  (2 #f "DATE" " 3 Jan 2015")
  (2 #f "AUTH" " Cari Lynn G")
  (illegal-line
   "<a href=\"/wiki/De_Ceredigion-1\" title=\"De Ceredigion-1\">De Ceredigion-1</a>\
 and Rhydderch-16 appear to represent the same person because: I think that these are\
 the same women.")))

(define example-data2
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
  ))

(define example-data2-result
  (list
   (ged-record
    "I2299" "INDI" ""
    (list
     (ged-record
      #f "NAME" " Jane  /Unknown/"
      (list (ged-record #f "_AKA" " Rhydderch" '())))
     (ged-record
      #f "BIRT" ""
      (list (ged-record #f "DATE" " ABT 1651" '())
            (ged-record #f "PLAC" " Alltgoch, Llanwenog, Cardiganshire, Wales" '())))
     (ged-record #f "DEAT" "" '())))
   (ged-record
    "I3299" "INDI" ""
    (list
     (ged-record
      #f "NAME" " Jane  /UnknoXwn/"
      (list (ged-record #f #"_AKA" " RhyddeXrch" '())))
     (ged-record
      #f "BIRT" ""
      (list (ged-record #f "DATE" " ABT 1651" '())
            (ged-record #f "PLAC" " Alltgoch, Llanwenog, Cardiganshire, Wales" '())))
     (ged-record #f "DEAT" "" '())))))

(check-equal? (first (parse-all-lines example-data2))
              (first example-data2-result))





(require rackunit)

(check-not-exn (λ () (record-parser 0 example-data)))

#;(define records (parse-records parsed-lines))