#lang racket



(require racket/fasl
         "shared.rkt"
         2htdp/image
         rackunit)

(printf "LOADING DATA\n")
(define top-hash
  (time
   (call-with-input-file "/tmp/xc-tree.fasl"
     (λ (port) (fasl->s-exp port)))))


(for/list ([k (in-list (hash-keys top-hash))])
  (list k (length (hash-ref top-hash k))))

(define individuals (hash-ref top-hash "INDI"))
(define person-table
  (make-immutable-hash
   (for/list ([i (in-list individuals)])
     (when (not (treenode-opt-xref-id i))
       (error 'no-xref-id "individual without an xref-id: ~e"
              i))
     (cons (treenode-opt-xref-id i)
           i))))
(unless (equal? (hash-count person-table)
                (length individuals))
  (error 'countcheck "multiple individuals with same key"))

(define family-groups (hash-ref top-hash "FAM"))
(define family-table
  (make-immutable-hash
   (for/list ([i (in-list family-groups)])
     (when (not (treenode-opt-xref-id i))
       (error 'no-xref-id "family group without an xref-id: ~e"
              i))
     (cons (treenode-opt-xref-id i)
           i))))

;; given a pointer to a person, return the pointers of their parents
(define (person-parents xref-id)
  (define person (hash-ref person-table xref-id))
  (match (hash-ref (treenode-elements person) "FAMC" '())
    ['() '()]
    [(list (treenode _ (list 'pointer ptr) _ _))
     (match (hash-ref family-table ptr)
       [(treenode _ _ fam-elements _)
        (define parents
          (append (hash-ref fam-elements "HUSB" '())
                  (hash-ref fam-elements "WIFE" '())))
        (for/list ([content (in-list (map treenode-line-content parents))])
          (match content
            [(list 'pointer p) p]))])]
    [(list-rest _ _ _)
     ;; figure out what to do about this...
     (error 'person-parents "more than one FAMC record")]))

(person-parents "I1")

;; compute the number of known nodes in a person's
;; family tree. double-counting is fine.
(define (known-ancestors xref-id)
  (+ 1 (apply + (map known-ancestors (person-parents xref-id)))))

(known-ancestors "I1")

(define generations 10)
(define width (expt 2 generations))

(rectangle width 2 "solid" "black")

;; given an xref-id, return 'M or 'F or 'X or 'U
(define (person-gender person-xref-id)
  (define sex
    (hash-ref (treenode-elements (hash-ref person-table person-xref-id))
              "SEX"
              '()))
  (match sex
    ['() 'U]
    [(list (treenode _ gstr _ _))
     (match gstr
       ["M" 'M]
       ["F" 'F]
       ["U" 'U]
       ["X" 'X])]))



(define (person-name person-xref-id)
  (define name-matches
    (hash-ref (treenode-elements (hash-ref person-table person-xref-id))
              "NAME"
              '()))
  (match name-matches
    ['() #f]
    [(list (treenode _ gstr _ _)) gstr]
    [other (error 'person-name "person ~a has more than one name: ~e"
                  person-xref-id (map treenode-line-content name-matches))]))

(define (person-color person-xref-id)
  (match (person-gender person-xref-id)
    ['M "blue"]
    ['F "pink"]
    ['X "yellow"]
    ['U "brown"]))



(define (draw-block width person-xref-id)
  (above
   (rectangle width 10 "solid" (person-color person-xref-id))
   (cond
     [(<= width 1) (rectangle 0 0 "solid" "white")]
     [else
      (define parents (person-parents person-xref-id))
      (match-define (list father mother)
        (match (map person-gender parents)
          ['() (list #f #f)]
          ['(M) (append parents '(#f))]
          ['(F) (append '(#f) parents)]
          [(or '(U) '(X)) (append parents '(#f))] ;; no gender, one known, use father side
          [(or '(M M) '(F F) '(X X) '(U U) '(X U) '(U X))
           (fprintf (current-error-port) "warning: no obvious parent ordering")
           parents]
          ;; first is male, 2nd female or unknown, or second is female & first unknown
          [(or '(M F) '(M U) '(M X) '(U F) '(X F)) parents]
          ;; same but reversed...
          [(or '(F M) '(F U) '(F X) '(U M) '(X M)) (reverse parents)]))
      (beside/align
       "top"
       (maybe-draw-block (/ width 2) father)
       (maybe-draw-block (/ width 2) mother))])))

(define (maybe-draw-block width person-or-false)
  (cond [person-or-false
         (draw-block width person-or-false)]
        [else
         (rectangle width 0 "solid" "white")]))

(draw-block (expt 2 10) "I1")

(for/fold ([width 1])
          ([i (in-range 10)])
  (+ 1 (* width 2)))

(define ((enumerate-brick-walls depth) person-xref-id)
  (define parents (person-parents person-xref-id))
  (define (motherof)
    (list 'mother-of depth (person-name person-xref-id)))
  (define (fatherof)
    (list 'father-of depth (person-name person-xref-id)))
  (define (partnerof parent-xref-id)
    (list 'parent-of depth (person-name person-xref-id)
          'partner-of (person-name parent-xref-id)))
  (define missing
    (match (map person-gender parents)
      ['() (list (motherof) (fatherof))]
      ['(M) (list (motherof))]
      ['(F) (list (fatherof))]
      [(or  '(X) '(U)) (list (partnerof (first parents)))]
      [(list a b) '()]))
  (apply
   append
   missing
   (map (enumerate-brick-walls (add1 depth)) parents)))



(define (list-brick-walls)
  (define brick-walls
    ((enumerate-brick-walls 0) "I1"))
  (printf "~a brick walls:\n" (length brick-walls))
  brick-walls)

;; a path is a chain of genders
;; seen will now map a name to a gender path
(define (multiply-reachable seen-people path person-xref-id)
  (cond [(hash-has-key? seen-people person-xref-id)
         (values (list (list person-xref-id path (hash-ref seen-people person-xref-id)))
                 seen-people)]
        [else
         (define new-seen (hash-set seen-people person-xref-id path))
         (define parents (person-parents person-xref-id))
         (for/fold ([dups '()] [seen new-seen])
                   ([p (in-list parents)])
           (define-values (new-dups new-seen)
             (multiply-reachable seen (cons (person-gender p) path) p))
           (values (append new-dups dups)
                   new-seen))]))

;; map a gender chain and a person to a name. So, if I gave you XC
;; and MMFF (male male female female) it would return Helen's xref-id
(define (gender-chain-end start-xref-id gender-chain)
  (cond [(empty? gender-chain)
         start-xref-id]
        [else
         (define parents (person-parents start-xref-id))
         (define chosen
           (filter (λ (p) (equal? (person-gender p) (first gender-chain)))
                   parents))
         (match chosen
           [(list)
            (error 'gender-chain-end "no parent of person ~v found with gender ~v"
                   start-xref-id (first gender-chain))]
           [(list parent)
            (gender-chain-end parent (rest gender-chain))]
           [(list parents ...)
            (error 'gender-chain-end "multiple parents of person ~v found with gender ~v"
                   start-xref-id (first gender-chain))])]))

(check-equal? (person-name (gender-chain-end "I1" '(M M F F)))
              "Alice Berry /Wright/")

(define the-person "I1")
(define-values (dups seen) (multiply-reachable (hash) '() the-person))

;; each dup is a list containing an xref-id and two reversed paths

(define (gender-list->str g)
  (apply string-append (map symbol->string g)))

;; given two paths and a known-common prefix, return the extended common prefix
;; and the different tails
(define (refactor p1 p2 prefix-so-far)
  (cond [(or (empty? p1) (empty? p2))
         (list (reverse prefix-so-far)
               p1
               p2)]
        [else
         (cond [(equal? (first p1) (first p2))
                (refactor (rest p1) (rest p2) (cons (first p1) prefix-so-far))]
               [else
                (list (reverse prefix-so-far)
                      p1
                      p2)])]))

(define refactored
  (group-by
  first
  (map (λ (dup)
         (match-define (list stem p1 p2)
           (refactor (reverse (second dup)) (reverse (third dup)) '()))
         (list (gender-list->str stem)
               (person-name (gender-chain-end the-person stem))
               (gender-list->str p1)
               (gender-list->str p2)
               (person-name (first dup))
               ))
       dups)))


refactored

#;(group-by
 (compose first second)
 (sort
  
  string<?
  #:key (compose first second))
 )

;; some people may occur 3 or more different ways....
(define supergrouped
  (map
   (λ (g) (list (first (first g))
                (remove-duplicates (apply append (map rest g)))))
   (group-by first dups)))

supergrouped

#;(
(take dups 3)
(map (λ (tup) (list (person-name (first tup))
                    (apply string-append (reverse (map ~a (second tup))))
                    (apply string-append (reverse (map ~a (third tup))))))
     dups))