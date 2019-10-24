;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |using lamdas for first time|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (add3-to-all empty) empty)
(check-expect (add3-to-all (list 1 2 3)) (list 4 5 6))
(define (add3-to-all alon)
  (map (λ (n) (+ n 3)) alon))

(check-expect (double-squares 0) empty)
(check-expect (double-squares 4) (list 0 2 8 18))
(define (double-squares n)
  (build-list n (λ (x) (* 2 x x))))

((λ (n) (+ n 3)) 5)
;the arg input is the 5
;8

(define (f n) (+ n 3))
(f 5)
;these are equivalent
;8

((λ(x y) (+ x y)) 3 9)
;12

((λ(x y) 4) 3 6)
;4

;;(λ (n)
;;  (if (= n 0) 1 (λ (sub1 n) (if (= (sub1 n) 0)))))

;;(define (short-msgs los)
;;  (filter (λ (msg) (< (string-length msg) SHORT-LEN)) los))

(check-expect (product-sqrts-plus-one (list 1 4 9)) 24)
(check-expect (product-sqrts-plus-one (list )) 1)

(define (product-sqrts-plus-one alon)
  ;(foldr (λ(n1 n2) (* (+ (sqrt n1) 1) n2)) 1 alon))
  (foldr * 1 (map (λ(n) (add1 (sqrt n))) alon)))

(check-expect (num-perfect-squares (list 1 4 3 5 9)) 3)
(check-expect (num-perfect-squares (list 7 3 5)) 0)
(check-expect (num-perfect-squares (list )) 0)
(define (num-perfect-squares alon)
  ;(length (filter (λ(n) (integer? (sqrt n))) alon))
  (foldr (λ(n1 n2)
           (if (integer? (sqrt n1))
               (add1 n2)
               n2))
         0 alon))