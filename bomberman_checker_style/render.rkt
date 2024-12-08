;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname render) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/vector)
(require racket/system)
(require racket/base)
(require "public.rkt")

(provide homepage)
(provide random-layout)
(provide homepage-state)
(provide initial-state)
(provide CELL-SIZE)

(provide count-num)
(provide render)

(define-struct gamestate [layout bomb player1 player2 roundtimer maximum quit?] #:transparent)
(define-struct bombstate [cor countdown owner] #:transparent)
(define-struct cor [column row] #:transparent)
(define-struct player1 [cor direction] #:transparent)
(define-struct player2 [cor direction] #:transparent)

;row col -> Boolean
;rules for generating 'U in random-layout
(define (is-U? row col)
  (and (even? row)          
       (odd? col)          
       (< 0 row (- MAX-ROWS 1))                   
       (< 0 col (- MAX-COLS 1))))

;row col -> Boolean
;rules for generating 'W1D in random-layout
(define (is-player1? row col)
  (= 0 (+ row col)))

;row col -> Boolean
;rules for generating 'W2U in random-layout
(define (is-player2? row col)
  (= (+ (- MAX-ROWS 1) (- MAX-COLS 1)) (+ row col)))

;row col -> Boolean
;rules for generating fixed 'W in random-layout
(define (is-fixed-W? row col)
  (or (and
       (< 0 (+ row col))
       (<= (+ row col) 2)) 
      (>= (+ row col) (+ (- MAX-ROWS 1) (- MAX-COLS 1) -2))))

;row col -> Boolean
;rules for generating 'D in homepage
(define (is-YU? row col)
  (or
   ;;Y shape
   (and
    (<= 0 col 2)
    (<= 0 row 2)
    (= col row))
   (and
    (<  2 col 6)
    (<= 0 row 2)
    (<= 2 (- col row) 4)
    (= (+ col row) 4))
   (and
    (= col 2)
    (<= 2 row 4))
   ;;U shape
   (and
    (= col 8)
    (<= 0 row 4))
   (and
    (= col 12)
    (<= 0 row 4))
   (and
    (= row 4)
    (<= 8 col 12))))

;row col -> Boolean
;rules for generating 'I in homepage
(define (is-HU? row col)
  (or
   ;;U shape
   (and
    (= col 8)
    (<= 6 row 10))
   (and
    (= col 12)
    (<= 6 row 10))
   (and
    (= row 10)
    (<= 8 col 12))
   ;;H shape
   (and
    (= col 0)
    (<= 6 row 10))
   (and
    (= col 4)
    (<= 6 row 10))
   (and
    (= row 8)
    (<= 0 col 4))))

;row col -> symbol
;rules for generate homepage
(define (homepage-rule row col)
  (cond
    [(is-YU? row col) 'D]
    [(is-HU? row col) 'I]
    [else 'W]))

;row col -> symbol
;rules for generate random-layout
(define (random-layout-rule row col)
  (cond
    [(is-player1? row col) 'W1D] ;generate the player1 at the top-left corner
    [(is-player2? row col) 'W2U] ;generate the player2 at the top-right corner
    [(is-U? row col) 'I] ;generate the fixed indestructible cell
    [(is-fixed-W? row col) 'W] ;generate the safe starting area :` and .: 
    [else (if (zero? (random 2)) 'D 'W)])) ;generate the random cell, destructible if 0, walkable if 1

;row col acc rule-fn -> vector of symbol
;generate row
(define (generate-row row col acc rule-fn)
  (if (= col MAX-COLS) ;if one row comes to the end
      (list->vector (reverse acc)) ;transform the acc list to vector
      (generate-row ;else
       row ;the row don't change
       (+ col 1) ;move to the next cell on the same row
       (cons (rule-fn row col) acc) ;new accumulator
       rule-fn)))
;;Test---missing---

;row acc rule-fn -> vector of vector of symbol
;generate layout
(define (generate-layout row acc rule-fn)
  (if (= row MAX-ROWS) ;if all rows come the end
      (list->vector (reverse acc))  ;tranform the list to vector
      (generate-layout ;else
       (+ row 1) ;change to the next row
       (cons (generate-row row 0 '() rule-fn) acc) ;column starts from 0, acc starts from '()
       rule-fn)))
;;Test---missing---

;homepage and random-layout
(define homepage (generate-layout 0 '() homepage-rule))
(define random-layout (generate-layout 0 '() random-layout-rule))

(define initial-state
  (make-gamestate
   random-layout
   '()
   initial-player1
   initial-player2
   initial-roundtimer
   initial-maximum
  #f))

(define homepage-state
  (make-gamestate
   homepage
   '()
   #f
   #f
   #f
   #f
  #f))

;constant definitions
(define CELL-SIZE (image-width (bitmap "W.png"))) ;the width of the walkable cell image

(define player1-image-D (bitmap "P1D.png"))
(define player1-image-U (bitmap "P1U.png"))
(define player1-image-L (bitmap "P1L.png"))
(define player1-image-R (bitmap "P1R.png"))

(define player2-image-D (bitmap "P2D.png"))
(define player2-image-U (bitmap "P2U.png"))
(define player2-image-L (bitmap "P2L.png"))
(define player2-image-R (bitmap "P2R.png"))

(define SPACE (square 30 0 "white"))

; render-cell
; symbol -> Image
(define (render-cell symbol)
  (crop 0 0 CELL-SIZE CELL-SIZE
        (cond
          [(symbol=? symbol 'D) (bitmap "D.png")]
          [(symbol=? symbol 'I) (bitmap "U.png")]
          [(symbol=? symbol 'W) (bitmap "W.png")]
          [(symbol=? symbol 'B) (bitmap "B.png")]
          [(symbol=? symbol 'E2) (bitmap "T.png")]
          [(symbol=? symbol 'E1) (bitmap "T.png")]
          [(symbol=? symbol 'E0) (bitmap "T.png")]
               
          

          ;player1
          [(symbol=? symbol 'W1L) (overlay player1-image-L (bitmap "W.png"))]
          [(symbol=? symbol 'W1R) (overlay player1-image-R (bitmap "W.png"))]
          [(symbol=? symbol 'W1U) (overlay player1-image-U (bitmap "W.png"))]
          [(symbol=? symbol 'W1D) (overlay player1-image-D (bitmap "W.png"))]
          [(symbol=? symbol 'B1L) (overlay player1-image-L (bitmap "B.png"))]
          [(symbol=? symbol 'B1R) (overlay player1-image-R (bitmap "B.png"))]
          [(symbol=? symbol 'B1U) (overlay player1-image-U (bitmap "B.png"))]
          [(symbol=? symbol 'B1D) (overlay player1-image-D (bitmap "B.png"))]
  

          ;player2
          [(symbol=? symbol 'W2L) (overlay player2-image-L (bitmap "W.png"))]
          [(symbol=? symbol 'W2R) (overlay player2-image-R (bitmap "W.png"))]
          [(symbol=? symbol 'W2U) (overlay player2-image-U (bitmap "W.png"))]
          [(symbol=? symbol 'W2D) (overlay player2-image-D (bitmap "W.png"))]
          [(symbol=? symbol 'B2L) (overlay player2-image-L (bitmap "B.png"))]
          [(symbol=? symbol 'B2R) (overlay player2-image-R (bitmap "B.png"))]
          [(symbol=? symbol 'B2U) (overlay player2-image-U (bitmap "B.png"))]
          [(symbol=? symbol 'B2D) (overlay player2-image-D (bitmap "B.png"))]

          ;player1-dead-image

          [(symbol=? symbol 'E1D) (overlay player1-image-D (bitmap "Boom.png"))]
          [(symbol=? symbol 'E1U) (overlay player1-image-U (bitmap "Boom.png"))]
          [(symbol=? symbol 'E1L) (overlay player1-image-L (bitmap "Boom.png"))]
          [(symbol=? symbol 'E1R) (overlay player1-image-R (bitmap "Boom.png"))]

          ;player2-dead-image

          [(symbol=? symbol 'E2D) (overlay player2-image-D (bitmap "Boom.png"))]
          [(symbol=? symbol 'E2U) (overlay player2-image-U (bitmap "Boom.png"))]
          [(symbol=? symbol 'E2L) (overlay player2-image-L (bitmap "Boom.png"))]
          [(symbol=? symbol 'E2R) (overlay player2-image-R (bitmap "Boom.png"))])))

; render-row:
(define (render-row layout)
  (let loop ( ;function name loop
             [i 0] ;start: i=0
             [acc empty-image] ;start: acc=empty-image
             )
    (if (>= i MAX-COLS)
        acc
        (loop (+ i 1)
              (beside acc (render-cell (vector-ref layout i)))))))        
;;Test---missing---

; render-layout
; layout -> Image
(define (render-layout layout)
  (let loop (
             [i 0]
             [acc empty-image]
             )
    (if (>= i MAX-ROWS)
         acc
        (loop (+ i 1) 
              (above acc (render-row (vector-ref layout i)))))))
;;Test---missing---

;convert-seconds-to-minutes-and-seconds-string:
;Number -> String
(define (seconds->minutes-and-seconds-string seconds)
  (let* (
        [minutes (quotient seconds 60)]
        [remaining-seconds (remainder seconds 60)]
        [length-remaining-seconds (string-length
                                   (number->string remaining-seconds))]
        )
  (string-append
   (number->string minutes)
   ":"
   (cond
     [(< length-remaining-seconds 2) (string-append "0" (number->string remaining-seconds))]
     [else (number->string remaining-seconds)]))))                   

;render-bar
;roundtimer maximum owner1 owner2 -> Image
(define (render-bar roundtimer maximum owner1 owner2)
  (beside
   (text (string-append "P1: " (number->string owner1)) 30 "indigo")
   SPACE
   (text (string-append "Roundtimer: " (seconds->minutes-and-seconds-string roundtimer)) 30 "indigo")
   SPACE
   (text (string-append "Maximum bomb: " (number->string maximum)) 30 "indigo")
   SPACE
   (text (string-append "P2: " (number->string owner2)) 30 "indigo")))

;count-num
(define (count-num list-of-bomb)
  (cond
    [(empty? list-of-bomb) 0]
    [else
     (add1 (count-num (rest list-of-bomb)))]))
 
;homepage-bar     
(define homepage-bar
   (text "Enter space to start game"
        30
        "indigo"))

;main render
;gamestate -> Image
(define (render gamestate)
  (let (
        [layout (gamestate-layout gamestate)]
        )
    (cond
      [(equal? layout homepage)
       (above
        homepage-bar
        (render-layout homepage))] ;layout of homepage
      [else
    
  (let* ( ;layout of game page
        [roundtimer (gamestate-roundtimer gamestate)]
        [maximum (gamestate-maximum gamestate)]
        [bomb-list (gamestate-bomb gamestate)]
        [owner1 (count-num (filter
                            (lambda(bombstate)
                              (symbol=? (bombstate-owner bombstate) 'P1))
                           bomb-list))]
        [owner2 (count-num (filter
                            (lambda(bombstate)
                              (symbol=? (bombstate-owner bombstate) 'P2))
                            bomb-list))]
        )
    (above (render-bar roundtimer maximum owner1 owner2)
           (render-layout (gamestate-layout gamestate))))])))

;;Test---missing---





