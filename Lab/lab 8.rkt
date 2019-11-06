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


;ex 2
;cross-product: [List-of X] [List-of Y] -> [List-of (list X Y)]
;length of result list = length of lox * length of loy
(define (cross-product lox loy)
  (cond [(empty? lox) empty]
        [(cons? lox) (append
                      (do-one-cross-product (first lox) loy)
                     (cross-product (rest lox) loy))]))

(define (crs-prd-2 lox loy)
  (foldr (λ (current sofar) (append (do-one-cross-product current loy) empty lox))
         empty lox))

;do-one-cross-product: X [List-of Y] -> [List-of (list X Y)]
;pairs x with each element in the list
;turning a list i nto another list of the same length
(define (do-one-cross-product x loy)
  (cond [(empty? loy) empty]
        [(cons? loy) (cons (list x (first loy)) ; this is the first element
                     (do-one-cross-product x (rest loy)))]))

(define (do-one-cross-product x loy)
  (map (λ (element-of-the-list) (list x element-of-the-list)) loy))

