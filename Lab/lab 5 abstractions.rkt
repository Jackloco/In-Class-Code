;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |lab 5 abstractions|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #t)))
; matching-x-posn : [List-of Posn] Number Posn -> Posn
; Find the first Posn in the list with the given x-coordinate or return the given Posn
; if no such position can be found
(check-expect (matching-x-posn empty 10 (make-posn 0 0)) (make-posn 0 0))
(check-expect
 (matching-x-posn
  (cons (make-posn 1 2) (cons (make-posn 3 4) empty)) 3 (make-posn 5 6))
 (make-posn 3 4))
(define (matching-x-posn lop desired-x default)
  (cond [(empty? lop) default]
        [(cons? lop)
         (if (= (posn-x (first lop)) desired-x)
             (first lop)
             (matching-x-posn (rest lop) desired-x default))]))
 
; string-with-length : [List-of String] Nat -> String
; Returns the first String in the given list with the given length or "no such string" if no
; such string can be found
(check-expect (string-with-length empty 10) "no such string")
(check-expect (string-with-length (cons "hi" (cons "hello" (cons "aloha" empty))) 5) "hello")
(define (string-with-length los desired-length)
  (cond [(empty? los) "no such string"]
        [(cons? los)
         (if (= (string-length (first los)) desired-length)
             (first los)
             (string-with-length (rest los) desired-length))]))

;ex 1
; matching-x-posn : [List-of Posn] Number Posn -> Posn
; string-with-length : [List-of String] Nat -> String
; the same
; take in a list of __ -> [Listof x]
; take in a desired number
; take in a default element if no match found
; different:
; type of the element in the list
; the function to turn an element into a number

; find-first-match: [Listof X] Number X [X->Number]->X
;finds the first element of the list matching the given number based on
;a supplied function that converts elements to a number
(define (find-first-match lox desired-value default x->num)
  (cond [(empty? lox) default]
        [(cons? lox)
         (if (= (x->num (first lox)) desired-value)
             (first lox)
             (find-first-match (rest lox) desired-value default))]))

(check-expect (find-first-match empty 10 (make-posn 0 0) posn-x)
              (make-posn 0 0 ))

;string-with-length.v2 : [List-of String] Nat -> String
(define (string-with-length.v2 los desired-length)
  (find-first-match los desired-length "no such string" string-length))

;ex 2
; A MaybeString is one of:
; - #false
; - String
 
; A MaybePosn is one of:
; - #false
; - Posn
;MaybeThing: false X -> X