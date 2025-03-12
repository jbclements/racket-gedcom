#lang racket

(require "parser.rkt"
         racket/fasl
         racket/runtime-path)

(define-runtime-path here ".")
  
(define top-hash
  (parse-gedcom (build-path here "examples" "lucy-dryer.ged")))

(call-with-output-file "/tmp/test.fasl"
  (λ (port)
    (s-exp->fasl top-hash port)))