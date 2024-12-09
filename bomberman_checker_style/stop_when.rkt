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

;end?:

;Input/Output:
;GameState -> Boolean
;
;Purpose Statement:
;Determines whether the game should end based on the current GameState.
;The game ends if any of the following conditions are met:
;--The player has chosen to quit the game (gamestate-quit? GameState) returns #t)
;--The RoundTimer reaches 0 
;--Both Players have died 
;--Player 1 has died 
;--Player 2 has died

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

;Input/Output:
;GameState -> Image

;Purpose Statement:
;Determines the final Image to display based on the GameState at the end of the game.
; - If the player has chosen to quit, returns quit-image.
; - If both Players have died or RoundTimer has finished, returns (tie GameState).
; - If only Player 1 has died, returns (player2-win GameState).
; - If only Player 2 has died, returns (player1-win GameState).
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

;player2-win Image:

;Input/Output:
;GameState -> Image

;Purpose Statement:
;Return the Player2-Win Image.
(define (player2-win gamestate)
  (overlay
   (text "PLAYER2-WIN!"
         100
         "RED")
   (render gamestate)))

;player1-win Image:

;Input/Output:
;GameState -> Image

;Purpose Statement:
;Return the Player1-Win Image.
(define (player1-win gamestate)
  (overlay
   (text "PLAYER1-WIN!"
         100
         "RED")
   (render gamestate)))


;tie Image:

;Input/Output:
;GameState -> Image

;Purpose Statement:
;Return the Tie Image.
(define (tie gamestate)
  (overlay
   (text "Tie!"
         70 
         "RED")
   (render gamestate)))

;check-player1-died?:

;Input/Output:
;Symbol -> Boolean

;Purpose Statement:
;Determines whether Player1 has died based on the given Symbol.
;Returns #t if the Symbol shows that Player1 is in an explosion.
;Returns #f otherwise.
(define (check-player1-died? symbol)
  (or
   (symbol=? symbol 'E1L)
   (symbol=? symbol 'E1R)
   (symbol=? symbol 'E1U)
   (symbol=? symbol 'E1D)))


;check-player2-died?:

;Input/Output:
;Symbol -> Boolean

;Purpose Statement:
;Determines whether Player2 has died based on the given Symbol.
;Returns #t if the Symbol shows that Player2 is in an explosion.
;Returns #f otherwise.
(define (check-player2-died? symbol)
  (or
   (symbol=? symbol 'E2L)
   (symbol=? symbol 'E2R)
   (symbol=? symbol 'E2U)
   (symbol=? symbol 'E2D)))

;check-all-died?:

;Input/Output:
;Symbol Symbol -> Boolean

;Purpose Statement:
;Determines whether Player1 and Player2 have died together.
;Returns #t if the Symbol shows that Player1 and Player2 are all in the explosion.
;Returns #f otherwise.

(define (check-all-died? symbol1 symbol2)
  (and (check-player1-died? symbol1) ;Player1 died
       (check-player2-died? symbol2))) ;Player2 died

  