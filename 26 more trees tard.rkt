;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |26 more trees tard|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (bst-temp abst)
  (cond [(string? abst) ...]
        [(branch? abst) ... (branch-key abst)
                        ...(branch-info abst)
                        ...(bst-temp (branch-left abst))
                        ...(bst-temp (branch-right abst))]))


;;retrieve: BST NatNum -> String
;get the info at the given key in the tree
(define (retrieve abst akey)
  (cond [(string? abst) (error "key not found")]
        [(branch? abst) (cond [(= akey (branch-key abst)) (branch-info abst)]
                              [(< akey (branch-key abst))
                               (retrieve (branch-left abst) akey)]
                              [else (retrieve (branch-right abst)akey)])]))

;bst->list: BST -> [List-of NatNum]
;preoduces a list of the keys in the tree
(check-expect (bst->list bst1) (list 7 8 10 12 13 14))
(check-expect (bst->list "leaf") empty)

(define (bst->list abst)
  (cond [(string? abst) empty]
        [(branch? abst) (append (list (branch-key abst))
                                (bst->list (branch-left abst))
                                (bst->list (branch-right abst)))]))

;bst? : BST -> Boolean
;is the tree a valid bst?
(check-expect (bst? bst1) true)
(check-expect (bst? bst1-bad) false)
(check-expect (bst? "leaf") true)
(define (bst? abst)
  (cond [(string? abst) true]
        [(branch? abst) (and (andmap (λ(k) (< k (branch-key abst)))
                                     (bst->list (branch-left abst)))
                             (andmap (λ(k) (> k (branch-key abst)))
                                     (bst->list (branch-right abst)))
                        (bst? (branch-left abst))
                        (bst? (branch-right abst)))]))