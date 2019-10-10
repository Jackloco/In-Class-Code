;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Abstractions) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #t)))
(define STR-13 "Yvcd9tlbZwhMN")
(define STR-14 "Yvcd9tlbZwhMN4")
(define STR-POL "dear so and so")
(define STR-POL2 "dear blah the second")

(define (short-msgs los)
  (cond
    [(empty? los) empty]
    [(cons? los)
     (if(< (string-length (first los)) 14)
        (cons(first los)) (short-msgs(rest los)))
     (short-msgs (rest los))]))