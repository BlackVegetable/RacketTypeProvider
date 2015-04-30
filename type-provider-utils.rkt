#lang racket

; Type Provider Utilities

(require rackunit)

(provide clear-void get-remainder-prefix hash-filter 
         prepend-$ primitive-value? string-char-position 
         string-infer symbol->keyword)

; Removes all #<void> elements from every layer of a list.
; (list -> list)
(define (clear-void lyst)
  (define (_clear-void elm processed)
    (cond
      [(cons? elm) (filter (λ (x)(not (void? x)))
                           (append (list (_clear-void (first elm) (list))) (_clear-void (rest elm) processed)))]
      [(empty? elm) processed]
      [else elm]))
  (_clear-void lyst (list)))

(check-equal? (clear-void (list 'a 'b 'c 'd))
              (list 'a 'b 'c 'd))
(check-equal? (clear-void (list 'a (void) 'c 'd))
              (list 'a 'c 'd))
(check-equal? (clear-void (list 'a (void) (void) (void)))
              (list 'a))
(check-equal? (clear-void (list 'a (list 'b 'c) 'd))
              (list 'a (list 'b 'c) 'd))
(check-equal? (clear-void (list 'a (list 'b (void)) 'd))
              (list 'a (list 'b) 'd))
(check-equal? (clear-void (list 'a (list (list 'b) (list (void) (void))(list 'c (void))) 'd))
              (list 'a (list (list 'b)(list)(list 'c)) 'd))

; Deteremines if a value is a number, string, boolean, or otherwise.
(define (primitive-value? x)
  (or (number? x)(string? x)(boolean? x)))

(check-equal? (primitive-value? "hello")
              #t)
(check-equal? (primitive-value? "")
              #t)
(check-equal? (primitive-value? (void))
              #f)
(check-equal? (primitive-value? #f)
              #t)

; Coerces a symbol into a keyword.
(define (symbol->keyword sym)
  (string->keyword (symbol->string sym)))

(check-equal? (symbol->keyword 'weight)
              '#:weight)
(check-equal? (symbol->keyword 'fancy-pants)
              '#:fancy-pants)

; Coerces a keyword into a symbol.
(define (keyword->symbol kw)
  (string->symbol (keyword->string kw)))

(check-equal? (keyword->symbol '#:fruit)
              'fruit)

;; symbol/keyword -> symbol/keyword
;; Prepends a dollar sign onto a symbol or keyword
(define (prepend-$ sym)
  (if (keyword? sym)
      (symbol->keyword (string->symbol (string-append "$" (symbol->string (keyword->symbol sym)))))
      (string->symbol (string-append "$" (symbol->string sym)))))

(check-equal? (prepend-$ 'money)
              '$money)
(check-equal? (prepend-$ '$double-dollar)
              '$$double-dollar)
(check-equal? (prepend-$ '#:velocity)
              '#:$velocity)

;; Returns the portion of full-word not matched by the prefix.
;; Prefix must not be longer than full-word.
;; String String -> String
(define (get-remainder-prefix prefix full-word)
  (define remainder-list (string-split full-word prefix))
  (cond 
    [(empty? remainder-list) ""]
    [else 
     (define divergence-position (- (string-length full-word) (string-length (first remainder-list))))
     (substring full-word divergence-position)]))

(check-equal? (get-remainder-prefix "hel" "hello")
              "lo")
(check-equal? (get-remainder-prefix "hello" "hello")
              "")

;; Determines if a given character is contained at least once in the given string.
;; If it does exist, returns the position within the string where it was first found.
;; If it doesn't, returns -1.
;; String Character -> Integer
(define (string-char-position str c [pos 0])
  (cond
    [(equal? str "") -1]
    [(equal? pos (string-length str)) -1]
    [(equal? (string-ref str pos) c) pos]
    [else (string-char-position str c (add1 pos))]))

(check-equal? (string-char-position "hello" #\h)
              0)
(check-equal? (string-char-position "hello" #\l)
              2)
(check-equal? (string-char-position "hello" #\w)
              -1)
(check-equal? (string-char-position "xk-cd" #\-)
              2)
(check-equal? (string-char-position "" #\A)
              -1)

;; Hash Procedure -> List
;; Applies filter to a Hash. The Procedure should expect two arguments: key and value.
;; The returned list is in an unspecified order.
(define (hash-filter h pred)
  (define lyst (hash->list h))
  (filter (λ (x) (pred (first x) (second x))) lyst))

(check-equal? (hash-filter (hash 'Aquarium '(λ ($$content
                                                #:$size [$size (void)]
                                                #:$water-quality [$water-quality (void)])
                                              (Aquarium $$content $size $water-quality))
                                 'Fish '(λ ($$content
                                            #:$hungry [$hungry (void)])
                                          (Fish $$content $hungry)))
                           (λ (k v) (equal? k 'Fish)))
              (list
               (cons 'Fish '(λ ($$content
                                #:$hungry [$hungry (void)])
                              (Fish $$content $hungry)))))
              
(define (string-whitespace? s)
  (andmap char-whitespace? (string->list s)))

(check-equal? (string-whitespace? "hello")
              #f)
(check-equal? (string-whitespace? "je ll o")
              #f)
(check-equal? (string-whitespace? "
") ; Intentionally ugly testcase.
              #t)
(check-equal? (string-whitespace? "  ")
              #t)

;; Conversions to be used for attribute values and content.
(define (string-infer s)
  (define num (string->number s))
  (define lower-s (string-downcase s))
  (if (not (equal? num #f))
      num
      (cond
        [(equal? lower-s "false") #f]
        [(equal? lower-s "true") #t]
        [(equal? s "") ""]
        [(string-whitespace? s) (void)]
        [else s])))

(check-equal? (string-infer "5")
              5)
(check-equal? (string-infer "0")
              0)
(check-equal? (string-infer "-5")
              -5)
(check-equal? (string-infer "234")
              234)
(check-equal? (string-infer "-13")
              -13)
(check-equal? (string-infer "-0") ; Strange case
              0)
(check-equal? (string-infer "0.123")
              0.123)
(check-equal? (string-infer "x") ; We won't use characters, just single character strings.
              "x")
(check-equal? (string-infer "w00t")
              "w00t")
(check-equal? (string-infer "800t5")
              "800t5")
(check-equal? (string-infer "sand")
              "sand")
(check-equal? (string-infer "two words")
              "two words")
(check-equal? (string-infer "")
              "")
(check-equal? (string-infer "true")
              #t)
(check-equal? (string-infer "True")
              #t)
(check-equal? (string-infer "TRUE")
              #t)
(check-equal? (string-infer "#t") ; Our XML probably won't use Racket syntax.
              "#t")
(check-equal? (string-infer "false")
              #f)
(check-equal? (string-infer "False")
              #f)
(check-equal? (string-infer "FALSE")
              #f)
(check-equal? (string-infer "#f") ; Our XML probably won't use Racket syntax.
              "#f")
(check-equal? (string-infer "\n")
              (void))
