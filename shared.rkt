#lang racket



(provide (struct-out treenode))


;; ooh, how to represent these parsed records? probably the best
;; thing is jsexprs: hashes with lists where it makes sense.
;; but also an outer struct for the line-content and the xref-id.
;; note that the tag isn't part of the parsed-thingy, just used
;; to construct the outer hash.

;; a ged-hash is a hash table mapping tag names to lists of treenodes
;; a treenode contains an optional xref-id (something a pointer points to),
;; the "line content" (a string, a pointer, or both), a ged-hash of
;; sub-elements, and the line number of the file on which the node occurs
(struct treenode (opt-xref-id line-content elements line-num) #:prefab)