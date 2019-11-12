;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |28 graphs|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct page [title links])
 
; A WebPage is a (make-page String [List-of String])
; Interpretation: a web page's title and links to other pages.

(define PAGE-0 (make-page "Khoury" (list "NEU")))
(define PAGE-1 (make-page "NEU" (list "Khoury" "Boston")))
(define PAGE-2 (make-page "Boston" (list "NEU" "Shillman Hall")))
(define PAGE-3 (make-page "Shillman Hall" (list "NEU")))
(define PAGE-4 (make-page "New Orleans" (list "Mardi Gras")))
 
(define (wp-temp wp)
  (... (page-title wp) ...
       (los-temp (page-links wp)) ...))
 
; A Wiki is a [List-of WebPage]
; Interpretation: A list of pages in a wiki
 
(define WIKI-1 (list PAGE-0 PAGE-1 PAGE-2 PAGE-3 PAGE-4))
 
(define (wiki-temp w)
  (...
   (cond
     [(empty? w) ...]
     [(cons? w)
      (...
       (wp-temp (first w)) ...
       (wiki-temp (rest w)) ...)])))

;num-pages: Wiki -> NatNum
;count the pages in a Wiki
(check-expect (num-pages (list PAGE-0)) 2)
(check-expect (num-pages WIKI-1) 6)
(define (num-pages w)
  (local [; all-pages: Wiki -> [List-of String]
          (define (all-pages w)
            (cond
              [(empty? w) empty]
              [(cons? w)
               (merge
                (cons (list (page-title (first w)))
                        (wp-temp (first w)))
                (all-pages (rest w)))]))]
    (length (all-pages w))))

;merge: [List-of X] [List-of X] -> [List-of X]
;merge the lists with no duplicates
(check-expect (merge (list "a" "b" "c") (list "c" "d")) (list "a" "b" "c" "d"))
(check-expect (merge (list ) (list "a")) (list "a"))
(check-expect (merge (list ) (list )) (list ))
(check-expect (merge (list "a") (list )) (list "a"))
(define (merge l1 l2)
  (cond [(empty? l1) l2]
        [(cons? l1)
         (if (member? (first l1) l2)
             (merge (rest l1) l2)
             (merge (rest l1) (cons (first l1) l2)))]))
