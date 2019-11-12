;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |lab 9|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Atom is one of:
; - Number
; - String
; - Boolean
 
; An SExpr is one of:
; - Atom
; - [List-of SExpr]


; atom? : Any -> Boolean
; Is the given data an Atom?
(check-expect (atom? 4) true)
(check-expect (atom? "hello") true)
(check-expect (atom? false) true)
(check-expect (atom? (list 1 2 3)) false)
(define (atom? x) (or (number? x) (string? x) (boolean? x)))

(define (sexpr-temp s)
  (cond [(atom? s)...]
        [(list? s) ... (LO-SEcpr-temp s) ...]))

(define (depth sex)
  (cond 
    [(empty? sex) 1]
    [(atom? sex) 1]
    [(cons? sex) (max (add1 (depth (first sex))) (depth (rest sex)))]))

(define (substitute s old new)
  (local [;replace-instance: Atom String String -> Atom
          ;replaces instances of old with new in the atom
          ;examples:
          ;7 "a" "b" -> 7,
          ;true "a" "b" -> true,
          ;"food" "a" "b" -> "foo"
          ;"foo" "foo" "bar" -> "bar"
          (define (replace-instance at old new)
            (cond [(string? at) (if (string=? old at) new at)]
                  [(number? at) at]
                  [(boolean? at) at]))]
    (cond [(atom? s) (replace-instance s old new)]
          [(list? s) (map (λ (elem) (sub elem old new)) s)])))


