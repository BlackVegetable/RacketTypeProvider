#lang racket

(require rackunit)

(provide (struct-out type-tree)
         match-criteria
         make-type-tree)

(struct type-tree
  (prev-chars ; string of prefix for this tree
   branches) ; list of type-trees.
  #:transparent)

;; string string -> boolean
;; Returns true if sub-str is a prefix of str.
(define (is-string-prefix? str sub-str)
  (define s-len (string-length str))
  (define sub-s-len (string-length sub-str))
  (and 
   (sub-s-len . <= . s-len)
   (equal? (substring str 0 sub-s-len) sub-str)))

(check-equal? (is-string-prefix? "Hello" "He")
              #t)
(check-equal? (is-string-prefix? "jello" "jelo")
              #f)
(check-equal? (is-string-prefix? "me" "me")
              #t)
(check-equal? (is-string-prefix? "derp42" "derp4200")
              #f)
  
;; string -> list of strings
;; Returns the left-to-right substrings of a string.
;; Does not include the empty string.
;; Includes a duplicate of the final word with a star appended to it.
(define (get-ordered-substrings s)
  (define (_get-ordered-substrings str acc) 
    (define strlen (string-length str))
    (if (equal? strlen 0)
        acc
        (_get-ordered-substrings (substring str 0 (sub1 strlen)) (append (list str) acc))))
  (if (equal? s "")
      (list)
      (append (_get-ordered-substrings s (list)) (list (string-append s "*")))))

(check-equal? (get-ordered-substrings "hello")
              (list "h" "he" "hel" "hell" "hello" "hello*"))
(check-equal? (get-ordered-substrings "")
              (list))

; (list sym ...) -> type-tree
; Constructs a type-tree out of a list of symbols.
(define (make-type-tree symbol-list)
  (define all-subwords (map (λ (x)(get-ordered-substrings (symbol->string x)))
                            symbol-list))
  (define (last-char-is-star str)
    (equal? (substring (list->string (reverse (string->list str))) 0 1) "*"))
  (define (remove-last-char str)
    (substring str 0 (sub1 (string-length str))))
  (define (make-subtree subwords-list)
    (cond [(empty? subwords-list) (list)]
          [else (begin (define first-chars (remove-duplicates (sort (map first subwords-list) string<?)))
                       (define rest-chars (filter (λ (x)(not (empty? x)))(map rest subwords-list)))
                       (map (λ (x)(if (last-char-is-star x)
                                      (type-tree (remove-last-char x) (list))
                                      (type-tree x (make-subtree 
                                                   (filter (λ (y)(is-string-prefix? (first y) x)) rest-chars)))))
                            first-chars))]))
  (type-tree "" (make-subtree all-subwords)))

(check-equal? (make-type-tree (list))
              (type-tree "" '()))

(check-equal? (make-type-tree (list 'at 'a 'atom))
              (type-tree "" (list (type-tree "a"
                                             (list (type-tree "a" '()) 
                                                   (type-tree "at"
                                                              (list (type-tree "at" '())
                                                                    (type-tree "ato"
                                                                               (list (type-tree "atom" (list
                                                                                                        (type-tree "atom" '()))))))))))))
                                                   

(define sample-tree
  (type-tree "" (list (type-tree "a" 
                                 (list (type-tree "an" '())
                                       (type-tree "at" '())))
                      (type-tree "b"
                                 (list (type-tree "be" '())))
                      (type-tree "t"
                                 (list (type-tree "to"
                                                  (list (type-tree "to" '())
                                                        (type-tree "too" '())
                                                        (type-tree "tow" '()))))))))

;; Gets all full words that are found in descendents of this tree.
;; Tree -> list of string
(define (get-all-sub-tree-words tree)
  (cond
    [(empty? (type-tree-branches tree)) (list (type-tree-prev-chars tree))]
    [else (flatten (map get-all-sub-tree-words (type-tree-branches tree)))]))

;; Finds all descendents of a TypeTree that the given string is a prefix of.
(define (match-criteria tree prefix [max-size 10])
  (define t-chars (type-tree-prev-chars tree))
  (define t-branches (type-tree-branches tree))
  (cond
    [((string-length t-chars) . > . (string-length prefix)) (get-all-sub-tree-words tree)]
    [(empty? t-branches) ; At the end of the subtree
     (cond
       [(equal? prefix t-chars) (list t-chars)]
       [else '()])]
    [(equal? (substring prefix 0 (string-length t-chars)) t-chars) ; Matches thus far
     (define all-results (flatten (map (λ (x) (match-criteria x prefix -1)) t-branches)))
     (if (max-size . > . 0) ; A negative max size is for internal use to not reduce the count of results.
         (take all-results (min (length all-results) max-size))
         all-results)]
    [else (list)]))

(check-equal? (match-criteria sample-tree "" 1)
              (list "an"))
(check-equal? (match-criteria sample-tree "" 22)
              (list "an" "at" "be" "to" "too" "tow"))
(check-equal? (match-criteria sample-tree "ax" 3)
              (list))
(check-equal? (match-criteria sample-tree "a" 3)
              (list "an" "at"))
(check-equal? (match-criteria sample-tree "at" 3)
              (list "at"))
(check-equal? (match-criteria sample-tree "b" 3)
              (list "be"))
(check-equal? (match-criteria sample-tree "to" 3)
              (list "to" "too" "tow"))
(check-equal? (match-criteria sample-tree "to" 2)
              (list "to" "too"))
(check-equal? (match-criteria sample-tree "too" 3)
              (list "too"))
