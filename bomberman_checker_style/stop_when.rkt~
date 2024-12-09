;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname stop_when) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/vector)
(require racket/system)
(require racket/base)
(require "public.rkt")
(require "render.rkt")

(provide (all-defined-out))

;constant definitions

;quit-image
(define quit-image
  (overlay
  (text "Thanks for playing!"
        70
        "red")
  (rectangle (* CELL-SIZE MAX-COLS)
             (* CELL-SIZE MAX-ROWS)
             "solid"
             "black")))


; end?:

; input/output:
; gamestate -> Boolean
;
; purpose statement:
; determines whether the game should end based on the current gamestate.
; the game ends if any of the following conditions are met:
; --the player has chosen to quit game (gamestate-quit? gamestate) returns #t)
; --the roundtimer reaches 0 
; --both players have died 
; --player 1 has died 
; --player 2 has died

(define (end? gamestate)
  (if (gamestate-quit? gamestate)
      #t
  (let (
        [layout (gamestate-layout gamestate)]
        )
    (cond
      [(equal? homepage layout) #f]
      [else
  (let* (
        [player1-cor (player1-cor (gamestate-player1 gamestate))]
        [player2-cor (player2-cor (gamestate-player2 gamestate))]
        [player1-symbol (get-symbol layout player1-cor)]
        [player2-symbol (get-symbol layout player2-cor)]
        [time-finish? (= 0 (gamestate-roundtimer gamestate))]
        )
    (or
      (check-all-died? player1-symbol player2-symbol)
      (check-player1-died? player1-symbol) 
      (check-player2-died? player2-symbol)
      time-finish?))]))))


;final:

;input/output:
;gamestate-> Image

;purpose statement:
;determines the final image to display based on the gamestate at the end of the game
; - if the player has chosen to quit, returns quit-image
; - if both players have died or roundtimer has finished, returns (tie gamestate)
; - if only Player 1 has died ,returns (player2-win gamestate)
; - if only Player 2 has died ,returns (player1-win gamestate)
(define (final gamestate)
  (if (gamestate-quit? gamestate)
      quit-image
  (let* (
        [layout (gamestate-layout gamestate)]
        [player1-cor (player1-cor (gamestate-player1 gamestate))]
        [player2-cor (player2-cor (gamestate-player2 gamestate))]
        [player1-symbol (get-symbol layout player1-cor)]
        [player2-symbol (get-symbol layout player2-cor)]
        [quit? (gamestate-quit? gamestate)]
        [time-finish? (= 0 (gamestate-roundtimer gamestate))]
        )
    (cond
      [(or
        (check-all-died? player1-symbol player2-symbol)
        time-finish?)
        (tie gamestate)]
      [(check-player1-died? player1-symbol) (player2-win gamestate)]
      [(check-player2-died? player2-symbol) (player1-win gamestate)]))))


;auxiliary functions:

;player2-win image:

;input/output:
;gamestate -> Image

;purpose statement:
;return the player2-win Image
(define (player2-win gamestate)
  (overlay
   (text "PLAYER2-WIN!"
         100
         "RED")
   (render gamestate)))

;player1-win image:

;input/output:
;gamestate -> Image

;purpose statement:
;return the player1-win Image
(define (player1-win gamestate)
  (overlay
   (text "PLAYER1-WIN!"
         100
         "RED")
   (render gamestate)))


;tie image:

;input/output:
;gamestate -> Image

;purpose statement:
;return the tie Image
(define (tie gamestate)
  (overlay
   (text "You Can Get Married!"
         70
         "RED")
   (render gamestate)))

;check-player1-died?:

;input/output
;symbol -> Boolean

;purpose statement:
;determines whether player1 has died based on the given symbol
;returns #t if the symbol shows that player1 is in an explosion 
;returns #f otherwise
(define (check-player1-died? symbol)
  (or
   (symbol=? symbol 'E1L)
   (symbol=? symbol 'E1R)
   (symbol=? symbol 'E1U)
   (symbol=? symbol 'E1D)))


;check-player2-died?:

;input/output
;symbol -> Boolean

;purpose statement:
;determines whether player2 has died based on the given symbol
;returns #t if the symbol shows that player2 is in an explosion 
;returns #f otherwise
(define (check-player2-died? symbol)
  (or
   (symbol=? symbol 'E2L)
   (symbol=? symbol 'E2R)
   (symbol=? symbol 'E2U)
   (symbol=? symbol 'E2D)))

;check-all-died?:

;input/output
;symbol1 symbol2 -> Boolean

;purpose statement:
;determines whether player1 and player2 has died together
;returns #t if the symbol shows that player1 and player2 are all in the explosion
;returns #f otherwise.

(define (check-all-died? symbol1 symbol2)
  (and (check-player1-died? symbol1)
       (check-player2-died? symbol2)))
  