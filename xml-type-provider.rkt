#lang at-exp racket

(require racket/runtime-path rackunit xml 
         (only-in "./type-provider-utils.rkt" clear-void symbol->keyword prepend-$))

(provide build-xml-type-provider
         just-constructors
         xml->constructors
         xml->structs)

;; These will keep the tool.rkt from choking on the testing paths.
(define-runtime-path HARD-PATH "test-xml.txt")
(define-runtime-path EASY-PATH "simpler-xml-test.txt")
(define-runtime-path DUP-PATH "simple-duplicate-xml.txt")
(define-runtime-path NOATTR-PATH "no-attribute-xml.txt")

(define sample-xml (read-xml (open-input-file EASY-PATH #:mode 'text)))
(define harder-xml (read-xml (open-input-file HARD-PATH #:mode 'text)))
(define duplicate-xml (read-xml (open-input-file DUP-PATH #:mode 'text)))
(define no-attr-xml (read-xml (open-input-file NOATTR-PATH #:mode 'text)))
                      

; Combines the contents of two hash objects into a single hash object.
; (hash hash -> hash)
(define (combine-hash h1 h2 [f (λ (k v h)(hash-set h k v))])
  (foldl f h1 (hash-keys h2)(hash-values h2)))

(check-equal? (combine-hash (hash 'a 1 'b 2)
                            (hash 'c 3))
              (hash 'a 1 'b 2 'c 3))
(check-equal? (combine-hash (hash)
                            (hash 'a 1))
              (hash 'a 1))

; Combines the element contents of two hash objects into a single hash object,
; retaining the elements with the most attributes.
(define (keep-biggest-elements h1 h2)
  (combine-hash h1 h2 (λ (k v h)(if (or (equal? 'not-present (hash-ref h k 'not-present))
                                        ((first (hash-ref h k)) . < . (first v)))
                                    (hash-set h k v)
                                    h))))

(check-equal? (keep-biggest-elements (hash 'a (list 1 'steve)
                                           'b (list 3 'murphy))
                                     (hash 'a (list 2 'steven)
                                           'c (list 2 'henry)))
              (hash 'a (list 2 'steven)
                    'b (list 3 'murphy)
                    'c (list 2 'henry)))



;; (xml hash -> hash)
;; Extends all-expr-hash to include any newly discovered xml-structs.
(define (xml->structs e all-expr-hash)
  (define (expand-element expr all-expr-hash)
    (define (xe->s expr) 
      (match expr
        [(? document? d) (xe->s (document-element d))]
        [(? element? e) (list 'struct
                              (xe->s (element-name e))
                              (cons '$$content
                                    (xe->s (element-attributes e)))
                              '#:transparent)]
        [(? attribute? a) (xe->s (prepend-$ (attribute-name a)))]
        [(list x ...) (map xe->s x)]
        [(? symbol? s) s] ; Names of elements and attributes
        [_ (void)]))
    (define res (clear-void (xe->s e)))
    (match (first res)
      [(? symbol? s) (hash-set all-expr-hash (second res) res)]
      [_ (foldl (λ (kv h)(hash-set h (second kv) kv)) all-expr-hash res)]))
  (match e
    [(? document? d)(xml->structs (document-element d) all-expr-hash)]
    [(? element? elm)(xml->structs (element-content elm)(expand-element elm all-expr-hash))]
    [(list x ...)
     (define elements-only (filter element? x))
     (define hash-list (map (λ (v)(xml->structs v all-expr-hash)) elements-only))
     (foldl combine-hash all-expr-hash hash-list)]
    [_ all-expr-hash]))

(check-equal? (xml->structs sample-xml (hash))
              (hash 'Aquarium '(struct Aquarium ($$content
                                                 $size
                                                 $water-quality)
                                 #:transparent)
                    'Fish '(struct Fish ($$content
                                         $hungry)
                             #:transparent)))

; Constructs a keyword + default void value s-expression for an
; XML attribute. 
(define (build-keyword-indicator attr)
  (define kwrd (symbol->keyword (prepend-$ (attribute-name attr))))
  (define formal `[(,(prepend-$ (attribute-name attr)) (void))])
  (cons kwrd formal))

(check-equal? (build-keyword-indicator (make-attribute #f #f 'weight "12"))
              '(#:$weight [$weight (void)]))

; Combines a list of two element lists into a list of the
; contents of each original list in the order they appeared
; in the original list.
(define (combine-two-element-lists list-of-lists [acc (list)])
  (if (empty? list-of-lists)
      (reverse acc)
      (combine-two-element-lists (rest list-of-lists)
                                 (list* (second (first list-of-lists)) (first (first list-of-lists)) acc))))

(check-equal? (combine-two-element-lists
               (list '(#:weight [weight (void)])
                     '(#:volume [volume (void)])
                     '(#:taste [taste (void)])))
              `(#:weight [weight (void)] #:volume [volume (void)] #:taste [taste (void)]))

; Produces constructors for the structs.
(define (xml->constructors e all-expr-hash)
  (define (expand-element expr all-expr-hash)
    (define (x->c expr)
      (match expr
        [(? document? d) (x->c (document-element d))]
        [(? element? e)
         (list (element-name e)
               (list
                (length (element-attributes e))
                (if (empty? (element-attributes e))
                    `(λ ($$content)(,(element-name e) $$content))
                    `(λ ,(list* '$$content (combine-two-element-lists
                                            (map build-keyword-indicator (element-attributes e))))
                       ,(list* (element-name e) '$$content (map prepend-$
                                                                (map attribute-name (element-attributes e))))))))]
        [_ (void)]))
    (define res (clear-void (x->c e)))
    (if (or (equal? 'not-present (hash-ref all-expr-hash (first res) 'not-present)) ; If this key is not already present
            ((first (hash-ref all-expr-hash (first res))) . < . (first (second res)))) ; If there are more attributes now
        (hash-set all-expr-hash (first res)(second res))
        all-expr-hash))
  (match e
    [(? document? d)(xml->constructors (document-element d) all-expr-hash)]
    [(? element? elm)(xml->constructors (element-content elm)(expand-element elm all-expr-hash))]
    [(list x ...)
     (define elements-only (filter element? x))
     (define hash-list (map (λ (v)(xml->constructors v all-expr-hash)) elements-only))
     (foldl keep-biggest-elements all-expr-hash hash-list)]
    [_ all-expr-hash]))

(check-equal? (xml->constructors no-attr-xml (hash))
              (hash 'Aquarium (list 0 '(λ ($$content)
                                         (Aquarium $$content)))
                    'Fish (list 1 '(λ ($$content
                                       #:$hungry [$hungry (void)])
                                     (Fish $$content $hungry)))))

(check-equal? (xml->constructors sample-xml (hash))
              (hash 'Aquarium (list 2 '(λ ($$content
                                           #:$size [$size (void)]
                                           #:$water-quality [$water-quality (void)])
                                         (Aquarium $$content $size $water-quality)))
                    'Fish (list 1 '(λ ($$content
                                       #:$hungry [$hungry (void)])
                                     (Fish $$content $hungry)))))

(check-equal? (xml->constructors duplicate-xml (hash))
              (hash 'Aquarium (list 2 '(λ ($$content
                                           #:$size [$size (void)]
                                           #:$water-quality [$water-quality (void)])
                                         (Aquarium $$content $size $water-quality)))
                    'Fish (list 3 '(λ ($$content
                                       #:$hungry [$hungry (void)]
                                       #:$smelly [$smelly (void)]
                                       #:$tasty [$tasty (void)])
                                     (Fish $$content $hungry $smelly $tasty)))))
                                     
; Converts the hash types of:
;   ATTRIBUTE-NAME : (list COUNT CONSTRUCTOR)
; to:
;   ATTRIBUTE-NAME : CONSTRUCTOR
(define (just-constructors expr-hash)
  (define hash-as-list (hash->list expr-hash))
  (make-immutable-hash (map (λ (x)(cons (first x)(third x))) hash-as-list)))

(check-equal? (just-constructors (xml->constructors sample-xml (hash)))
              (hash 'Aquarium '(λ ($$content
                                   #:$size [$size (void)]
                                   #:$water-quality [$water-quality (void)])
                                 (Aquarium $$content $size $water-quality))
                    'Fish '(λ ($$content
                               #:$hungry [$hungry (void)])
                             (Fish $$content $hungry))))

(define (build-xml-type-provider c-input-path)
  (define ctime-xml (read-xml (open-input-file c-input-path #:mode 'text)))
  (define ctime-table (just-constructors (xml->constructors ctime-xml (hash))))
  `(begin
     (require (only-in xml read-xml))
     ,@(hash-values (xml->structs ctime-xml (hash))) ; Struct definitions
     (define runtime-struct-table
       ,(cons 'hash
              (apply append
                     (hash-map ctime-table
                               (λ (name function)
                                 (list `(quote ,name)
                                       function))))))
     (define populate-at-runtime (λ (r-input-path)
                                   (xml->populator 
                                    (read-xml (open-input-file r-input-path #:mode 'text))
                                    runtime-struct-table)))))
