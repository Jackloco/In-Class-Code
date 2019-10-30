;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 24-append) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;example design a function append-lists that accepts two lists

(check-expect (append-lists empty empty) empty)
(check-expect (append-lists empty (list 1 2)) (list 1 2))
(check-expect (append-lists  (list 1 2) empty) (list 1 2))
(check-expect (append-lists  (list "a") (list 1 2)) (list "a" 1 2))


(define (append-lists l1 l2)
  (cond [(empty? l1) l2]
        [(cons? l1)
         (cons (first l1) (append-lists (rest l1) l2))]))

