;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |26 trees|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct person [name parent1 parent2])
;a person is one of:
;-false
;-(make-person String PersonTree PersonTree)

(define PERSON-0 false)
(define ALICE (make-person "alice" PERSON-0 PERSON-0))
(define CAROL (make-person "carol" PERSON-0 PERSON-0))
(define BOB (make-person "bob" CAROL PERSON-0))
(define SALLY (make-person "sally" ALICE BOB))

(define (person-temp p)
  (cond [(boolean? p) ...]
        [(person? p) ...(person-temp (person-parent1 p))
                     ... (person-temp (person-parent2 p))]))

;;count-persons: PersonTree -> NatNum
; count the persons in the tree
(check-expect (count-persons PERSON-0) 0)
;;no people in tree
(check-expect (count-persons SALLY) 4)
;sally is origin point for the tree, so it includes everyone and origin point
(define (person-temp p)
  (cond [(boolean? p) 0]
        [(person? p) (+ 1(person-temp (person-parent1 p))
                        (person-temp (person-parent2 p)))]))

;depth: PersonTee -> NatNum
;compute the depth of the tree
(check-expect (depth PERSON-0) 0)
(check-expect (depth SALLY) 3)
(check-expect (depth BOB) 2)
(define (person-temp p)
  (cond [(boolean? p) 0]
        [(person? p) (add1 (max (depth (person-parent1 p));;max to make sure we get the deepest side of the tree
                                (depth (person-parent2 p))))]));;need to add1 still at current position
(define-struct three-tree (data left middle right))
;a 3-tree is one of:
;-false
;-(make-three-tree Number 3-tree 3-tree 3-tree)
;interpretation: A 3-ary tree

(define 3-TREE-1 false)
(define 3-TREE-2 (make-three-tree 2 3-TREE-1 3-TREE-1 3-TREE-1))
(define 3-TREE-3 (make-three-tree 3 3-TREE-2 3-TREE-1 3-TREE-1))

(define (3t-temp 3t)
  (cond [(boolean? 3t) ...]
        [(three-tree? 3t)...(three-tree-data 3t)
                         ...(3t-temp (three-tree-left 3t))
                         ...(3t-temp (three-tree-middle 3t))
                         ...(3t-temp (three-tree-right 3t))]))

;; A Webpage is a (make-page String [List-of WebPage])
; Interpretation: a web page's title and links to other pages

(define PAGE-0 (make-page "Khoury" empty))
(define PAGE-1 (make-page "NEU" (list PAGE-0)))
(define PAGE-2 (make-page "Boston" (list PAGE-0 PAGE-1)))

;;exists?: Webpage String -> Boolean
;is there a webpage with the given title?
(check-expect (exists? PAGE-0 "Boston") false)
(check-expect (exists? PAGE-2 "Khoury") true)
(check-expect (exists? PAGE-2 "Cambridge") false)
(check-expect (exists? PAGE-2 "NEU") false)
(define (exists? wp name)
  (local [;[List-of Webpage] -> Boolean
          ;check if the name is in a webpage in the list
          (define (exists-in-list? wpl)
            (ormap (λ(x) (exists? x name)) wpl))]
    (or (string=? (page-title wp) name)
        (exists-in-list? (page-links wp)))))

;exists-in-list?: [List-of Webpage] String -> Boolean
;is there a page with the given title in the list?
(check-expect (exists-in-list? empty "Khoury") false)
(check-expect (exists-in-list? (list PAGE-0 PAGE-1) "Khoury") true)
(check-expect (exists-in-list? (list PAGE-0 PAGE-1) "Boston") false)
(define (exists-in-list? wpl name)
  (cond [(empty? wpl) false]
        [(cons? wpl) (or (exists? (first wpl) name)
                         (exists-in-list? (rest wpl) name))]))

