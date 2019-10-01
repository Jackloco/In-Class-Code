;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 4|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #t)))
;ex 4 lab
(define NAT 0)
;;(define NAT1 (add1 NAT0)) ; =1
;;(define NAT2 (add1 NAT1)) ; =2
;n is equal to (add1 (1 less than n))
;so the parts of n are (add1 ...) and (sub1 n)
;nat-temp: NAT -> ???
#;(define (nat-temp n)
  (cond [(zero? n) ...]
        [(positive? n)
         (...(add1 ... (nat-temp (sub1 n )) ...) ...)]))

;ex5
;even-nat? : Nat -> Boolean
;determines whether a number is even or not
;(check-expect (even-nat? 0) true)
;(check-expect (even-nat? 1) false)
;(check-expect (even-nat? 2) true) ; 2 is (add1 (add1 0))
;(check-expect (even-nat? 1000) true)
;(check-expect (even-nat? 2000) true)
;(check-expect (even-nat? (add1 (add1 0))) (not (not true)))
;(define (nat-temp n)
;  (cond [(zero? n) true]
;        [(positive? n)
;         (not (even-nat? (sub1 n)))]))

;ex6
;nat+: Nat Nat -> Number
;takes in two nats and produces the sum
(check-expect (nat+ 12 10) 22)
(check-expect (nat+ 9 10) 19)
(check-expect (nat+ 0 10) 10)
(check-expect (nat+ 12 0) 12)
(check-expect (nat+ 12 12) 24)

(define (nat+ n1 n2)
  (cond [(zero? n2) n1]
        [(zero? n1) n2]
        [(= n1 n2) (* n1 2)]
        [(> n1 n2) (nat+ (add1 n1) (sub1 n2))]
        [(> n2 n1) (nat+ (add1 n2) (sub1 n1))]))

;nat+v.2: Nat Nat -> Nat
(define (nat+.v2 n1 n2)
  (cond [(zero? n1) n2]
        [(positive? n1)
         (add1(nat+.v2(sub1 n1) n2))]))

(define center (make-posn 100 100))
(define background (rectangle 200 200 "solid" "black"))

(define-struct ring [color size others])
; A RingSet is one of:
; - "no rings"
;    and represents the starting state where nothing is on the screen
; - (make-ring Color Nat RingSet)
;    and represents an underlying circle of a given color and radius

(define (ringRing state)
  (big-bang
      [to-draw draw-ring]
    [on-tick next]
    []