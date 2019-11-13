;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |28 graphs|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct page [title links])

;; A WebPage.v1 is a (make-page String [List-of WebPage])
;; and represents a web page's title and links to other pages

(define PAGE0.v1 (make-page "Khoury" empty))
(define PAGE1.v1 (make-page "NEU" (list PAGE0.v1)))
(define PAGE2.v1 (make-page "Boston" (list PAGE0.v1 PAGE1.v1)))

;; A WebPage is a (make-page String [List-of String])
;; and represents a web page's title and the titles of the pages it links to

(define PAGE0 (make-page "Khoury" (list "NEU")))
(define PAGE1 (make-page "NEU" (list "Khoury")))
(define PAGE2 (make-page "Boston" (list "Khoury" "NEU")))
(define PAGE3 (make-page "Shillman Hall" (list "NEU")))
(define PAGE4 (make-page "New Orleans" (list "Mardi Gras")))

;; webpage-template : WebPage -> ???
(define (webpage-template wp)
  (... (page-title wp)
       (list-template (page-links wp)) ...))

;; A Wiki is a [List-of WebPage]
;; and represents all the pages in an online wiki

(define WIKI-EMPTY empty)
(define WIKI-FULL (list PAGE0 PAGE1 PAGE2 PAGE3 PAGE4))

;; wiki-template : Wiki -> ???
(define (wiki-template w)
  (cond [(empty? w) ...]
        [(cons? w)
         (... (webpage-template (first w))
              (wiki-template (rest w)) ...)]))

;; num-pages : Wiki -> Nat
;; Produces the total count of pages mentioned (whether they exist or not)
(check-expect (num-pages WIKI-EMPTY) 0)
(check-expect (num-pages WIKI-FULL) 6)
(define (num-pages w)
  (length (all-pages w)))

;; all-pages : Wiki -> [List-of String]
;; Produces a unique list of all the pages mentioned (whether they exist or not)
(check-expect (all-pages WIKI-EMPTY) empty)
(check-expect
 (same-set? (all-pages WIKI-FULL)
            (list "Khoury" "NEU" "Boston" "Shillman Hall" "New Orleans" "Mardi Gras"))
 true)
(define (all-pages w)
  (local [;; add-unique-pages : WebPage [List-of String] -> [List-of String]
          ;; Add unique pages from the given webpage to the given list
          (define (add-unique-pages wp sofar)
            (add-unique (cons (page-title wp) (page-links wp)) sofar))]
    (foldr add-unique-pages empty w)))

;; add-unique : [List-of String] [List-of String] -> [List-of String]
;; Add any new strings from the first list into the second list
(check-expect (add-unique empty empty) empty)
(check-expect (add-unique empty (list "a" "b")) (list "a" "b"))
(check-expect (add-unique (list "c" "d") empty) (list "c" "d"))
(check-expect
 (same-set? (add-unique (list "a" "b" "c" "d") (list "a" "c" "e"))
            (list "a" "b" "c" "d" "e"))
 true)
(define (add-unique los1 los2)
  (foldr (λ (s1 sofar) (if (string-in-list? s1 sofar) sofar (cons s1 sofar)))
         los2
         los1))

;; string-in-list? : String [List-of String] -> Boolean
;; Is the given string in the given list?
(check-expect (string-in-list? "hello" empty) false)
(check-expect (string-in-list? "b" (list "a" "b" "c")) true)
(define (string-in-list? str los)
  (ormap (λ (s) (string=? str s)) los))

;; same-set? : [List-of String] [List-of String] -> Boolean
;; Do these lists contain the same strings in any order?
(check-expect (same-set? empty empty) true)
(check-expect (same-set? empty (list "a" "b" "c")) false)
(check-expect (same-set? (list "1" "2") empty) false)
(check-expect (same-set? (list "a" "b" "c" "d") (list "d" "a" "b" "c")) true)
(define (same-set? los1 los2)
  (and (andmap (λ (s1) (string-in-list? s1 los2)) los1)
       (andmap (λ (s2) (string-in-list? s2 los1)) los2)))

;; lookup : String Wiki -> [List-of String]
;; Get the links for the page with the given name (or empty if the page does not exist)
(check-expect (lookup "anything" WIKI-EMPTY) empty)
(check-expect (lookup "Boston" WIKI-FULL) (list "Khoury" "NEU"))
(define (lookup ptitle wiki)
  (cond [(empty? wiki) empty]
        [(cons? wiki)
         (if (page-has-title? (first wiki) ptitle)
             (page-links (first wiki))
             (lookup ptitle (rest wiki)))]))

;; page-has-title? : WebPage String -> Boolean
;; Does the given page have the given title?
(check-expect (page-has-title? PAGE0 "Winter") false)
(check-expect (page-has-title? PAGE4 "New Orleans") true)
(define (page-has-title? wp ptitle)
  (string=? (page-title wp) ptitle))

;; connected? : String String Wiki -> Boolean
;; Are the two pages with the given names connected?
(check-expect (connected? "Something" "Anything Else" WIKI-EMPTY) false)
(check-expect (connected? "Shillman Hall" "Khoury" WIKI-FULL) true)
(define (connected? start end wiki)
  (local [(define connections (lookup start wiki))]
    (or (string-in-list? end connections)
        (ormap (λ (neighbor) (connected? neighbor end wiki)) connections))))