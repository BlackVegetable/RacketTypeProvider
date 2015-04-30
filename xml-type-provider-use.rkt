#lang racket

(require "./xml-type-provider-macro.rkt" "./xml-type-provider.rkt")

;; Example usage follows.
;;
;; Note, both the macro and the base file must be required.
;; Also, the there must be a newline between the #lang xyz and the
;; (macro:gen-type-provider "./test-xml.txt")
;; Furthermore, the argument to this macro must be a string literal, not
;; an expression that evaluates to a string literal.
;;
;; The ugliest problem right now is that the data needs to be present in
;; the /Racket folder or an absolute path to it is required.
;; Hopefully, these limitations will be removed in a future version.

;; Tells Dr. Racket what XML to use for autocomplete.
;; Also satisfies background compilation. To try the autocomplete, type
;; a single letter, like "a" without the quotes and hit F8. Then,
;; click anywhere in the terrible GUI box to get your autocomplete
;; options. After it fills in your selection, type a "-" again
;; without the quotes and repeat the process to see what attributes
;; that element has.
(macro:gen-type-provider "./test-xml.txt")

;; Used to define the data you would like to access at runtime.
;; This is allowed to vary from the data used for the macro, but
;; the shape of the XML must not deviate in certain ways. An example
;; of an illegal deviation is an XML element containing an attribute
;; that is not present in any instance of that element in the macro
;; data.
(define subject (populate-at-runtime "./test-xml.txt"))

;; Handy little function similar to (first (filter predicate a-list))
;; Useful for the Type Provider because the macro has no knowledge
;; of the exact "content" of an element, only its attributes.
;; Thus, by filtering for a uniquely named sub-element within the
;; content, you can access a sub-element that you believe exists.
(define (f-filter pred lyst)
  (define res (filter pred lyst))
  (if (empty? res)
      (void)
      (first res)))

;; Example selects an element from the content of the largest XML layer.
;; In this example, the top-layer element is called "enterprise".
(define sel (first (enterprise-$$content subject)))

;; In reality, the exact ordering of the data is likely to vary.
;; Either a cond statement like the one below or a function like
;; f-filter should be used to select a sub-element.
(cond
  ;; Selects the timeframe within the group, displaying all of its attributes.
  [(group? sel) (f-filter timeframe? (group-$$content sel))]
  
  ;; Selects only the email address. Here, content resolves to a string instead of
  ;; another element.
  [(person? sel) (email-$$content (f-filter email? (person-$$content sel)))]
  [else "Not a group or person."])
