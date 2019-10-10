;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Abstractions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define STR-13 "Yvcd9tlbZwhMN")
(define STR-14 "Yvcd9tlbZwhMN4")
(define STR-POL "dear so and so")
(define STR-POL2 "dear blah the second")

#;(define (short-msgs los)
  (cond
    [(empty? los) empty]
    [(cons? los) (if(< (string-length (first los)) 14)
        (cons(first los)) (short-msgs(rest los)))
     (short-msgs (rest los))]))


;;find-sum: [List-of Number] -> Number
;Finds the sum of the numbers in the list

#;(define (find-sum lon)
  (cond
    [(empty? lon) 0]
    [(cons? lon)
     (cons? lon)
     (+
      (first lon)
      (find-sum (rest lon)))]))


;find-product: [List-of Number]-> Number
;;finds the product
#;(define (find-product lon)
  (cond
    [(empty? lon) 1]
    [(cons? lon)
     (cons? lon)
     (*
      (first lon)
      (find-sum (rest lon)))]))

;combine: [List-of Number] [Number Number -> Number] Number -> Number
;last chunk is base case
;first two are what we put in
;(check-expect (combine LON-3 *1) 24)
;(check-expect (combine LON-0 *1) 1)
(check-expect (combine (cons (cons 1 (cons 2 empty)) (cons (cons 3 (cons 4 empty)) empty))
                       append empty)
              (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
(define (combine lon op base)
  (cond
    [(empty? lon) base]
    [(cons? lon)
     (op
      (first lon)
      (combine (rest lon) op base))]))