;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |abstraction practice|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #t)))
;add-7-to-all: [List of Number] -> [List of Number]
;adds 7 to every number in the list
(check-expect (add-7-to-all empty) empty)
(check-expect (add-7-to-all (list 1 2 3)) (list 8 9 10))
(define (add-7-to-all alon)
  ;; [Number -> Number] [List-of Number] -> [List-of Number]
  (map add7 alon))

;add7: Number->Number
;adds 7 to the number
(check-expect (add7 1) 8)
(define (add7 n) (+ n 7))