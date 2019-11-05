;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |lab 8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;alternate : (x) [List-of X] [List-of X] -> [List-of X]
;intertwines the two lists

(define (alternate list-one list-two)
  (cons [(and (empty? list-one) (empty? list-two)) empty]
        [(and (cons? list-one) (empty? list-two)) list-one]
        [(and (empty? list-one) (cons? list-two)) list-two]
        [(and (cons? list-one) (cons? list-two))
         (cons (first list-one)
               (cons (first list-two)
                     (alternate (rest list-one)
                                (rest list-two))))]))

;case 1: operate on one thing, just pass the other along
(define (alternate.v2 l1 l2)
  (cons [(empty? l1) l2]
        [(cons? l1)
         (cons (first l1)
               (alternate.v2 l2 (rest l1)))]))
;;swicthes the args back and forth to make in leu of an actual code choosing back and forth