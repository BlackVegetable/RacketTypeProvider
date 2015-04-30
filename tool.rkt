#lang racket/unit

(require 
  drracket/tool
  framework
  racket/class
  racket/gui/base
  racket/list
  racket/string
  "./type-tree.rkt"
  (only-in "./type-provider-utils.rkt"
           get-remainder-prefix
           hash-filter
           string-char-position)
  (only-in xml
           read-xml)
  (only-in "./xml-type-provider.rkt"
           just-constructors
           xml->constructors
           xml->structs))

(import drracket:tool^)
(export drracket:tool-exports^)

(define ctime-xml #f) ; Value to store cached XML read.
(define data-trie #f) ; Value to store cached data-trie results.
(define last-used-path #f) ; Check to see if the path in the macro has been changed.

(define f (new frame%
                 [label "Click/F8 for Names"]
                 [width 250]
                 [height 250]))

(define menu (new popup-menu%))

(new (class canvas%
       (super-new [parent f])
       (inherit popup-menu)
       (define/override (on-event e)
         ;; Popup menu on button click:
         (when (send e button-down?)
           (popup-menu menu (send e get-x) (send e get-y))))
       (define/override (on-char e)
         ;; Popup menu on space bar or f8:
         (define key-pressed (send e get-key-code)) ; Something odd with key presses needs to be fixed. TODO
         (when (or (equal? #\space key-pressed)
                   (equal? 'f8 key-pressed))
           (popup-menu menu 10 10)))))

;; Get the current/previous s-expression
(define (get-current-prefix frame)
  (define current-cursor-position (send frame get-start-position))
  (define start-exp-position (send frame get-backward-sexp current-cursor-position))
  (if (equal? #f start-exp-position)
      ""
      (send frame get-text start-exp-position current-cursor-position)))

;; Struct-String Hash-of-Structs -> TypeTree
;;
;; Builds an XML type tree containing only the attributes of a given XML element name.
(define (build-attribute-type-tree elm-name ctime-table)
  (define matching-list (hash-filter ctime-table (λ (k v) (equal? (string->symbol elm-name) k))))
  (cond [(empty? matching-list) (make-type-tree (list))]
        [else (define matching-struct-body (fourth (first matching-list)))
              (define attributes (rest matching-struct-body)) ; Discard struct name.
              (make-type-tree attributes)]))

;; Where the magic happens!
;; Displays the autocomplete/GUI elements and determines what text needs to be
;; appended onto the existing s-expression to complete it to the selected name.
;; GUI-frame Tab Hash -> (void)
(define (show-text-box frame definitions ctime-table)
  (define (add-item name display-name prefix)
    (new menu-item%
         [label display-name]
         [parent menu]
         [callback (lambda (m e) (begin (send definitions insert (get-remainder-prefix prefix name))
                                        (send f show #f)))]))
  (map (λ (x) (send x delete)) (send menu get-items)) ; Clear existing menu.
  (define prefix (get-current-prefix frame))
  (define position-of-hyphen (string-char-position prefix #\-))
  (cond
    [(equal? position-of-hyphen -1) ; No hyphen found.
     (begin (map (λ (x) (add-item x x prefix)) (match-criteria data-trie prefix))
            (send f show #t))]
    [else
     (define after-hyphen-prefix (substring prefix (add1 position-of-hyphen)))
     (define att-type-tree (build-attribute-type-tree (substring prefix 0 position-of-hyphen) ctime-table))
     (define matching-attributes (match-criteria att-type-tree after-hyphen-prefix))
     (begin
       (define up-to-hyphen-prefix (substring prefix 0 (add1 position-of-hyphen)))
       (map (λ (x) (add-item (string-append up-to-hyphen-prefix x) x prefix)) matching-attributes)
       (send f show #t))]))

;; XML -> Hash-of-constructors
(define (get-ctime-table ctime-xml)
  (just-constructors (xml->constructors ctime-xml (hash))))
     
;; Editor-Input-Port -> String-Filepath/eof
(define (get-macro-filepath d-port)
  (define current-datum (read d-port))
  (cond [(and (list? current-datum)
              (equal? (first current-datum) 'macro:gen-type-provider))
         (second current-datum)]
        [(eof-object? current-datum) current-datum]
        [else (get-macro-filepath d-port)]))

;; File-Path -> XML
(define (get-xml-from-path p)
  (read-xml (open-input-file p #:mode 'text)))

;; Editor-Frame -> Void
;;
;; Display context specific structure information to the developer.
(define (type-provider-display frame)
  (define definitions (send (send frame get-tab) get-defs))
  (define d-port (open-input-text-editor definitions))
  (define current-path (begin (read-line d-port) ; Discard #lang xyz.
                              (get-macro-filepath d-port)))
  (cond [(eof-object? current-path) (void)]
        [else
         (cond 
           [(or (equal? #f data-trie) ; Never been initialized.
                (not (equal? last-used-path current-path))) ; New path given.
            (begin
              (set! last-used-path current-path)
              (set! ctime-xml (get-xml-from-path current-path))
              (set! data-trie (build-xml-type-tree ctime-xml)) ; Refactor away this set! TODO
              (show-text-box frame definitions (get-ctime-table ctime-xml)))]
           [else (show-text-box frame definitions (get-ctime-table ctime-xml))])]))
        
;; XML -> TypeTree
;;
;; Converts an X-Expression to a Type-Tree.
(define (build-xml-type-tree ctime-xml)
  (define struct-names (hash-keys (xml->structs ctime-xml (hash))))
  (make-type-tree struct-names))

(define type-provider-mixin
  (mixin (editor:keymap<%>) ()
    (define/override (get-keymaps)
      (let ([kmap (make-object keymap:aug-keymap%)])
        (send kmap map-function "f8" "use type provider")
        (send kmap add-function "use type provider" (λ (a b)(type-provider-display this)))
        (cons kmap (super get-keymaps))))
    (super-new)))

(define (phase1) (void))
(define (phase2) (void))

(drracket:get/extend:extend-definitions-text type-provider-mixin)