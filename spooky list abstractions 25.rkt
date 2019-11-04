;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |spooky list abstractions 25|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 1: design the function rev that accepts a list and reverses it.
; You must use a pre-defined list abstraction.

(define (rev lox)
  (foldl cons empty lox))
;combines first thing in list then second then third
;reverse order

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 2: produce signature, implement function

; prizes : [List-of String] [List-of Number] -> [List-of Strings]
; Awards each person in the first list a prize amount from the second;
; if run out of prizes, person gets 0 :(


(check-expect (prizes (list "alice" "bob" "carol") (list 100 50 10))
              (list "alice gets $100"
                    "bob gets $50"
                    "carol gets $10"))
 
#|
(check-expect (prizes (list "alice" "bob" "carol" "dan") (list 100 50))
              (list "alice gets $100"
                    "bob gets $50"
                    "carol gets $0"
                    "dan gets $0"))
|#
 
(check-expect (prizes (list "alice" "bob") (list 100 50 10))
              (list "alice gets $100"
                    "bob gets $50"))


(define (prizes names amounts)
  (local [;string number -> string
          ;makes a string for the prize announcement
          (define (make-announcement s n)
            (string-append s " gets $" (number->string n)))]
  (cond [(and (empty? names) (empty? amounts)) empty]
        [(and (empty? names) (cons? amounts)) empty]
        [(and (cons? names) (empty? amounts)) (cons(make-announcement(first amounts) 0)
                                             (prizes names (rest amounts)))]
        [(and (cons? names) (cons? amounts))
         (cons (make-announcement (first names)
         (first amounts))
         (prizes (rest names) (rest amounts)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 3: consider the following data definition...

; A [Checklist Z] is a [List-of [Z -> Boolean]]
; Interpretation: A series of tests

; Design the function checklist->predicate that accepts a [Checklist Z]
; and returns a *single* predicate [Z -> Boolean] that only returns true
; if *all* predicates in the checklist return true.
; You must use a pre-defined list abstraction.

; checklist->predicate : (Z) [Checklist Z] -> [Z -> Boolean]
; Returns a predicte that holds true only if all predicates return true


(check-expect (filter (checklist->predicate empty)
                      (list -2 -1 0 1 2))
              (list -2 -1 0 1 2))

(check-expect (filter (checklist->predicate (list even? positive?))
                      (list -2 -1 0 1 2))
              (list 2))

(check-expect (filter (checklist->predicate (list zero? even?))
                      (list -2 -1 0 1 2))
              (list 0))



(define (checklist->predicate cl)
  (local [; [Z-> Boolean] [Z->Boolean] -> [Z->Boolean]
          ;combines the predicates into one predicate
          (define (combine-pred p? q?)
            (λ(z) (and (p? z) (q? z))))]
   (foldr combine-pred (λ(x) true) cl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 4: design the function mysort that accepts a [List-of Number] and
; returns a sorted version of that [List-of Number].
; Use a list abstraction, but not sort.

; Core idea: given an already sorted list of numbers, and a new number,
; you can create a new list of sorted numbers by finding the right spot
; for the new number. But where is that? Well, since the list is already
; sorted, the first time the new value is less than a number in the list,
; you have found it's spot! For example...

; Given empty (which is sorted!) and 10: (list 10) is sorted
; Given (list 10) and 5: 5 < 10, so (list 5 10) is sorted
; Given (list 5 10) and 7: 5 < 7, but 7 < 10, so (list 5 7 10) is sorted


; mysort : [List-of Number] -> [SortedList-of Number]
; Sorts a list of numbers

(define (mysort lon)
  (...
   (list-abstraction ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 5: design the function make-uno-deck that accepts a Nat representing
; the highest numeric card value and a [List-of String] representing the colors,
; and generates a deck of Uno cards (as a [List-of UnoCard]). There should be one
; card with each color and number.
; Do so first via templates and then with list abstractions.
; Hint: are there more than one complex inputs driving the function?

(define-struct card [number color])

; An UnoCard is a (make-card Number String)
; representing a card for uno with a number and color
 
(define UNOCARD-1 (make-card 1 "red"))
(define UNOCARD-2 (make-card 2 "blue"))
 
(define (unocard-temp uc)
  (... (card-number uc) ...
       (card-color uc) ...))
 
; make-uno-deck : Nat [List-of String] -> [List-of UnoCard]
; Makes a deck of Uno cards, with one card of each color and number

#|
(check-expect (make-uno-deck 3 (list "red" "green"))
              (list (make-card 1 "red") (make-card 2 "red") (make-card 3 "red")
                    (make-card 1 "green") (make-card 2 "green") (make-card 3 "green")))
|#

(define (make-uno-deck n colors)
  ...)