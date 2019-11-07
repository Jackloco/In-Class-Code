;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |27 mutually|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct ol [content])
; A OL is a (make-ol [List-of Div])
 
(define-struct ul [content])
; A UL is a (make-ul [List-of Div])
 
; A Div is one of:
; - String
; - Image
; - OL
; - UL
; Interpretation: The structure of an HTML page,
; where OL is an ordered list and UL is an unordered list
 
(define DIV-1 "Hi!")
(define DIV-2 "Hello!")
(define DIV-3 (make-ul (list DIV-1 DIV-2)))
(define DIV-4 (make-ol (list DIV-1 DIV-2 DIV-3)))

(define (ol-temp ol)
  ... (div-list-temp (ol-content ol)) ...)

(define (ul-temp ul)
  ... (div-list-temp (ul-content ul))...)

(define (div-temp d)
  (cond [(string? d)...]
        [(image? d)...]
        [(ol? d)...(div-list-temp (ol-content d))]
        [(ul? d) ...(div-list-temp (ul-content d))]))

(define (div-list-temp dl)
  (cond [(empty? dl) ...]
        [(cons? dl) ... (first dl)
                    ...(div-list-temp (rest dl))]))

;;num-divs: Div-> NatNum
; count the number of divs
(check-expect (num-divs DIV-1) 1)
(check-expect (num-divs DIV-3) 3)
(check-expect (num-divs DIV-4) 6)

(define (num-divs d)
  (cond [(string? d) 1]
        [(image? d) 1]
        [(ol? d) (add1 (counts-in-list (ol-content d)))]
        [(ul? d) (add1 (counts-in-list (ul-content d)))]))

;counts-in-list: [List-of Div] -> NatNum
;count the divs in the list
(check-expect (counts-in-list (list DIV-1 DIV-2)) 2)
(check-expect (counts-in-list (list DIV-1 DIV-2 DIV-3)) 5)
(define (div-list-temp dl)
  (cond [(empty? dl) 0]
        [(cons? dl) (+ (num-divs (first dl)
                    (counts-in-list (rest dl))))]))