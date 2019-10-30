;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 24-get-element) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-error (get-element empty 0))
;(error "no element at this index")
(check-expect (get-element (list 1 2 4) 0) 1)
(check-expect (get-element (list 1 2 4) 1) 2)
(check-expect (get-element (list 1 2 4) 2) 4)
(check-expect (get-element (list 1 2 3) 3) "Index out of bounds")
(check-expect (get-element empty 3) "Index out of bounds")


;get-element: [List-of Any] Nat -> Any
;Gets the element in the specified location in the list,
;or raises an error if invalid

(define (get-element loa n)
        (cond [(and (empty? loa) (zero? n)) (error "Index out of bounds")]
              [(and (empty? loa) (> n 0)) (error "Index out of bounds")]
              [(and (cons? loa) (zero? n)) (first loa)]
              [(and (cons? loa) (> n 0)) (get-element (rest loa) (sub1 n))]))