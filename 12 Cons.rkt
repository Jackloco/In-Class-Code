;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |12 Cons|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #t)))
(define L0 empty)
(define L1 (cons 1 L0))
(define L2 (cons 1 (cons 2 L0)))

(define (list-temp alon)
  (cond [(empty? alon)...]
        [(cons? alon)...(first alon)
                     ... (list-temp (rest alon))]))