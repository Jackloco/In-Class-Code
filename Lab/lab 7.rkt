;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |lab 7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (f x y z)
  (local [(define x (+ y z))]
    (local [(define y z)]
      (local [(define z (* x y))]
        (+ x y z)))))
 
(check-expect (f 1 2 3) 23)
(check-expect (f 4 3 2) 17)
 
(define (g a b)
  (+ (local [(define b (string->number a))]
       (local [(define a (sqr b))]
         (- a b)))
     (string-length a)
     (if b 10 5)))
;(+ (- (sqr b) (string->number a)) (string-length a) (if b 10 5))

(check-expect (g "5" true) 31)
(check-expect (g "3" false) 12)
;