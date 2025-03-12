#lang racket

(require "parser.rkt"
         racket/fasl
         racket/runtime-path)

(define-runtime-path here ".")

;; sadly, I'm concerned that including a WikiTree-generated gedcom file
;; with this distribution, even one for people that have all been dead for
;; two hundred years, may be a violation of copyright. Sigh.
(define top-hash
  (parse-gedcom (build-path here "examples" "lucy-dryer.ged")))

(call-with-output-file "/tmp/test.fasl"
  (λ (port)
    (s-exp->fasl top-hash port)))