;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |11 Moons and Ice|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #t)))
(define (system-temp s)
  (...
   (cond
     [(boolean? s) ...]
     [(satellites? s ) ...
      (moon-temp (satellities-moon s)) ...
      (system-temp (satellites-remaining s))...])))

;;draw-eclipse: System -> IMage
;;draws the moons onto the sky
;;design process in play
(check-expect (draw-eclipse false)
              (place-image SUN HALF HALF SKY))
(check-expect (draw-eclipse SYSTEM-2)
              (place-image MOON 300 HALF
(define (draw-eclipse s)
  SKY)
