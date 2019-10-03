;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lots of different types of data definitions|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #t)))
(define-struct post [user body])

; A Post is a (make-post String String)
; Interpretation: Represents the username and text of a post

(define POST-DECORUM (make-post "TheMod" "Just please be respectful."))
(define POST-DUMB (make-post "silly_bear" "sounds dumb lol"))
(define POST-BANNED (make-post "TheMod" "The above user has been banned."))
(define POST-CENSORSHIP (make-post "free_speech" "STOP CENSORING ME! lol"))
(define POST-SPAM (make-post "free_speech" ""))

#;(define (post-temp very)
  ...(post-user very)... (post-body very)...)

; A ListOfPosts (LoP) is one of:
; - empty
; - (cons Post LoP)
; Interpretation: a list of posts

#;(define (LoP-temp really)
  (cond
    [(empty? really)...]
    [(cons? really)...(post-temp (first really))
                   ...(lop-temp (rest really))]))
;;the rest of the list needs to be dealt with so it calls itself again

(define LOP-0 empty)
(define LOP-DECORUM (cons POST-DUMB (cons POST-BANNED LOP-0)))
(define LOP-CENSORSHIP (cons POST-SPAM LOP-0))


(define-struct thread [title pinned? post replies])

; A Thread is a (make-thread String Boolean Post LoP)
; Interpretation: Represents the name, whether or not it is a pinned thread,
; the initial post, and replies of a thread

(define THREAD-DECORUM (make-thread "Message Bored Decorum" true POST-DECORUM LOP-DECORUM))
(define THREAD-CENSORSHIP (make-thread "CENSORS!!!" false POST-CENSORSHIP LOP-CENSORSHIP))

#;(define (thread-temp needle)
  ...(thread-title needle)
  ...(thread-pinned? needle)
  ...(post-temp (thread-post needle))
  ...(lop-temp (thread-replies needle))...)

; A ListOfThreads (LoT) is one of:
; - empty
; - (cons Thread LoT)
; Intepretation: a list of threads

(define LOT (cons THREAD-DECORUM (cons THREAD-CENSORSHIP empty)))

;;copied lop template to edit
#;(define (lot-temp much)
  (cond
    [(empty? much)...]
    [(cons? much)...(thread-temp (first much))
                   ...(lot-temp (rest much))]))

(define-struct mb [name threads])

; A MessageBoard is a (make-mb String LoT)
; Interpretation: Represents a the name and threads of a message board

(define MESSAGE-BOARD (make-mb "Generic Message Bored" LOT))
(define MESSAGE-BOARD-1 (make-mb "Empty message board" empty))
;;empty lot will give 0 pinned comments

#;(define (mb-temp amb)
  ...(mb-name amb) ...(lot-temp (mb-threads amb)))

;;design a function that counts the number of
;;pinned threads in a message board

;; count-pinned-threads: MessageBoard -> NatNum
;; count the pinned threads in the message board

;;the two posts wayy at the top only have 1 pinned
(check-expect (count-pinned-threads MESSAGE-BOARD) 1)
(check-expect (count-pinned-threads MESSAGE-BOARD-1) 0)

(define (count-pinned-threads amb)
  (count-pinned-threads-in-list (mb-threads amb)))
;;don't dissesmble lists in here, make a helper

;count-pinned-threads-in-list: LoT -> NatNum
;count the pinned threasd in the list
(check-expect (count-pinned-threads-in-list empty) 0)
(check-expect (count-pinned-threads-in-list LOT) 1)
(define (count-pinned-threads-in-list much)
  (cond
    [(empty? much) 0]
    [(cons? much)(+(if (thread-pinned? (first much)) 1 0)
                   (count-pinned-threads-in-list (rest much)))]))

;;design a function the determines whether any
;;post in a message board has an empty body

;has-empty-body-post?: MessageBoard -> Boolean
;checks if the messageboard has a post with an empty body
(check-expect (has-empty-body-post? MESSAGE-BOARD-1) true)
(check-expect (has-empty-body-post? MESSAGE-BOARD) true)

(define (has-empty-body-post? amb)
  (check-for-empty-post? (mb-threads amb)))
;;not finished
