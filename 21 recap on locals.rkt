;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |21 recap on locals|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define x 1)
(define y 3)

(define z (local [(define x 10)
                  (define 20)]
            (local [(define x 30)]
              (+x y))))
(+ x z)

;double-squares : Nat -> [List-of Nat]
;the first n double squares
(check-expect (double-squares 0) empty)
(check-expect (double-squares 4) (list 0 2 8 18))

(define (double-squares n)
  (local [;Nat -> Nat
          ;doubles the square of n
          (define (f n)
            (* 2 n n))]
    (build-list n f)))

;design function that adds 3 to every number in a list
;[List-of Number] -> [List-of Number]
;adds 3 to every umber in the list
(check-expect (add3-to-all empty) empty)
(check-expect (add3-to-all (list 1 2 3)) (list 4 5 6))
(define (add3-to-all alon)
  (local [;Number-> Number
          ;adds 3 to the number
          ;given: 2, produces: 5
          ;given: 3, produces: 6
          (define (add3 n) (+ 3 n))]
    ;[Number -> Number] [List-of Number] -> [List-of Number]
    (map add3 alon)))


;shortest-string: [List-of String] -> SoF
;returns the (first) shortest string, or false if given an empty list
(define (shortest-string los)
  (local [;String SoF -> SoF
          ;gets the shortest, or the string if SoF is false
          ;given: "foo" false, produces "foo"
          ;given: "foo" "a", produces "a"
          ;given: "f" "a", produces "a"
          ]
    ;[String -> SoF] SoF [List-of String] -> SoF
    (foldr gets-shortest false los)))


(check-expect (USD-to-EUR (list 1 2 3) 2.0) (list 2.0 4.0 6.0))
(define (USD-to-EUR lon rate)
  (local [;Number -> Number
          ;converts a dollar amount to euros
          ;given: 2 abnd rate is 2, produces 4 
          (define (convert-one n) (* n rate))]
    ;[Number -> Number] [List-of Number] -> [List-of Number]
    (map convert-one lon)))


(check-expect (slope (make-posn 1 2) (make-posn 0 0)) 2.0)
(define (slope p1 p2)
  (local [(define RISE (- (posn-y p2) (posn-y p1)))
          (define RUN (- (posn-x p2) (posn-x p1)))]
    (/ RISE RUN)))


