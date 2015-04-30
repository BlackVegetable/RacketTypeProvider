#lang racket

(require (for-syntax "xml-type-provider.rkt") xml "./type-provider-utils.rkt" rackunit)

(provide macro:gen-type-provider xml->populator)

; Uses a runtime hash-table of element names to functions to produce
; run-time struct instances.
(define (xml->populator expr rtime-hash)
    (match expr
      [(? document? d) (xml->populator (document-element d) rtime-hash)]
      [(? element? e)
       (define content (clear-void (xml->populator (element-content e) rtime-hash)))
       (define attribs-keywords (map (λ (z)(prepend-$ (symbol->keyword (attribute-name z))))(element-attributes e)))
       (define attribs-values (xml->populator (element-attributes e) rtime-hash))
       (clear-void (keyword-apply (hash-ref rtime-hash (element-name e))
                                  attribs-keywords
                                  attribs-values
                                  (cond 
                                    [(empty? content) (list content)]
                                    [(primitive-value? (first content)) (list (first content))]
                                    [else (list content)])))]
      [(? attribute? a)(string-infer (attribute-value a))]
      [(list x ...)(map (λ (y)(xml->populator y rtime-hash)) x)]
      [(? pcdata? p)(string-infer (pcdata-string p))]
      [_ (void)]))

(define-syntax (macro:gen-type-provider stx)
  (syntax-case stx ()
    [(_ c-path)
     (datum->syntax stx 
                    (build-xml-type-provider (syntax->datum #'c-path))
                    stx)]))
