;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname render) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/vector)
(require racket/system)
(require racket/base)
(require "public.rkt")
(require rackunit)
(require rackunit/text-ui)

(provide (all-defined-out))

; render-cell
; symbol -> Image
(define (render-cell symbol)
  (crop 0 0 CELL-SIZE CELL-SIZE
        (cond
          [(symbol=? symbol 'D) (bitmap "decorations/D.png")]
          [(symbol=? symbol 'I) (bitmap "decorations/I.png")]
          [(symbol=? symbol 'W) (bitmap "decorations/W.png")]
          [(symbol=? symbol 'B) (overlay (bitmap "decorations/B.png") (bitmap "decorations/W.png"))]
          [(symbol=? symbol 'E1) (bitmap "decorations/E.png")]
          [(symbol=? symbol 'E0) (bitmap "decorations/E.png")]

          ; player1
          [(symbol=? symbol 'W1L) (overlay player1-image-L (bitmap "decorations/W.png"))]
          [(symbol=? symbol 'W1R) (overlay player1-image-R (bitmap "decorations/W.png"))]
          [(symbol=? symbol 'W1U) (overlay player1-image-U (bitmap "decorations/W.png"))]
          [(symbol=? symbol 'W1D) (overlay player1-image-D (bitmap "decorations/W.png"))]
          [(symbol=? symbol 'B1L) (overlay player1-image-L (bitmap "decorations/B.png"))]
          [(symbol=? symbol 'B1R) (overlay player1-image-R (bitmap "decorations/B.png"))]
          [(symbol=? symbol 'B1U) (overlay player1-image-U (bitmap "decorations/B.png"))]
          [(symbol=? symbol 'B1D) (overlay player1-image-D (bitmap "decorations/B.png"))]

          ; player2
          [(symbol=? symbol 'W2L) (overlay player2-image-L (bitmap "decorations/W.png"))]
          [(symbol=? symbol 'W2R) (overlay player2-image-R (bitmap "decorations/W.png"))]
          [(symbol=? symbol 'W2U) (overlay player2-image-U (bitmap "decorations/W.png"))]
          [(symbol=? symbol 'W2D) (overlay player2-image-D (bitmap "decorations/W.png"))]
          [(symbol=? symbol 'B2L) (overlay player2-image-L (bitmap "decorations/B.png"))]
          [(symbol=? symbol 'B2R) (overlay player2-image-R (bitmap "decorations/B.png"))]
          [(symbol=? symbol 'B2U) (overlay player2-image-U (bitmap "decorations/B.png"))]
          [(symbol=? symbol 'B2D) (overlay player2-image-D (bitmap "decorations/B.png"))]

          ; player1-dead-image
          [(symbol=? symbol 'E1D) (overlay player1-image-D (bitmap "decorations/Boom.png"))]
          [(symbol=? symbol 'E1U) (overlay player1-image-U (bitmap "decorations/Boom.png"))]
          [(symbol=? symbol 'E1L) (overlay player1-image-L (bitmap "decorations/Boom.png"))]
          [(symbol=? symbol 'E1R) (overlay player1-image-R (bitmap "decorations/Boom.png"))]

          ; player2-dead-image
          [(symbol=? symbol 'E2D) (overlay player2-image-D (bitmap "decorations/Boom.png"))]
          [(symbol=? symbol 'E2U) (overlay player2-image-U (bitmap "decorations/Boom.png"))]
          [(symbol=? symbol 'E2L) (overlay player2-image-L (bitmap "decorations/Boom.png"))]
          [(symbol=? symbol 'E2R) (overlay player2-image-R (bitmap "decorations/Boom.png"))])))


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






