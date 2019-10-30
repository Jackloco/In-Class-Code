;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 24-intersect) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;interesect: (x) [List-of X] [List-of X] [X X -> Boolean] -> [List-of X]
;returns the subset of the two lists that are common given an equality relation

(check-expect (interesct (list "a" "b" "c") l(list "c" "d" "a") string=?) (list "a" "c"))

(define (intersect l1 l2 same?)
  (local [;contained-in?: x [list-of x] -> boolean
          ;is x in the list?
          (define (contained-in? x alox)
            (ormap (λ(x2) (same? x x2)) alox))]
    (filter (λ(x2) (contained-in? x2 l2)) l1)))
          

(define (intersect l1 l2 same?)
  (local [;;x [list-of x] -> boolean
          ;is the element in the list?
          ;given: 3, (list 1 2 3), =; expect true
          ;given: 3  (list 1 2), =; expect false
          ;given: 3, (list ), =; expect false
          (define (contained-in? x lox)
            (cond [(empty? lox) false]
                  [else (or (same? x (first x))
                            (contained-in? x (rest x)))]))
          ;[list-of x] -> [list-of x]
          ;keeps only those in the list that are in l2
          (define (check-each alox)
            (cond [(empty? alox) empty]
                  [(cons? alox)
                   (if (contained-in? (first alox) l2)
                       (cons (first alox) (check-each (rest alox)))
                       (check-each (rest alox)))]))]
    (check-each l1)))