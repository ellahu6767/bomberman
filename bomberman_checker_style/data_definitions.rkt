;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname data_definitions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/base)

;gamestate is a structure
(define-struct gamestate [layout bomb player1 player2 roundtimer maximum quit?] #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;layout:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;data types:
;layout is one of the following:
; -- (Vector of (Vector of Symbol)) 

;;;;;;;;;;;;;;;;;interpretation for (Vector of (Vector of Symbol));;;;;;;;;;;;;;;;;;;;;;;

;the layout of the game is a 2D vector grid composed of same-size-square-shape cells.
;each cell contains a symbol.
;each symbol represents a specific symbol in the game. 


;for each symbol:
;'W represents the walkable cell
;'I represents the undestructible cell
;'D represents the destructible cell
;'B represents the cell with unexploded bomb

;for each three-length symbol
;it represents one of the player on the kinds of cell with one direction
;for example, 'W1L represents player1 on the 'W cell with left direction

;for each two-length symbol
;'E2 'E1 'E0 represents the exploding bomb and its countdown
;for example, 'E2 means one exploding bomb and the exploding will last 2 seconds.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;layout:;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bomb ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;bomb is one of the following:
; -- '()  ;; no existing boomstate in game
; -- list of bombstate structure
; -- #f in homepage

;interpretation
;represents all of the existing bombstate
;including both exploding and unexploed bomb

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bombstate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;bombstate is a structure
(define-struct bombstate [cor countdown owner] #:transparent)
;interpretation:

;;;;;;;;;;;;;;;;;;;;;;;;;;;; countdown ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;countdown is an Integer in the interval
;from 3
;to 0(included)

;interpretation:
;represents the countdown of the each bombstate
;3 is the time for bomb just added to game 
;0 is the time for bomb start to explode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;cor is a structure
(define-struct cor [column row] #:transparent)
;(make-cor Integer Integer)

;interpretation:
;represents the cell location in the game
;column is a Integer , represents the location of column
;row is a Integer, represents the location of row


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;bomb;;;;;;;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player1:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;data types:
;player1 is one of the following:
(define-struct player1 [cor direction] #:transparent)
;-- (make-player1 cor Boolean String)
;-- #f in homepage

;interpretation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;cor:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;cor represents the position of player1



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;direction:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;direction is a String
;represents the the direction of player1


;example
(define initial-player1 (make-player1
                        (make-cor 0 0)
                        "D"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player1;;;;;;;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;data types:
;player2 is one of the following:
(define-struct player2 [cor direction] #:transparent)
;-- (make-player2 cor String)
;-- f in homepage

;interpretation is as similar as player1
;just replace all "player1" with "player2"

;example
(define initial-player2 (make-player2
                        (make-cor (- MAX-COLS 1) (- MAX-ROWS 1))
                        "U"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player2;;;;;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;roundtimer:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;data types:
;roundtimer is one of the following:
;-- roundtimer is a Interger in the interval:
;  ;-- from 120(included) to 0(included)
;-- #f in homepage

;interpretation
;represents the countdown of one game

;examples;
(define initial-roundtimer 120)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;roundtimer;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;maximum;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;maximum is one of the following:
;maximum is an Interger in the interval:
;  ;--from 3(included) to 6(included)
;--#f in homepage

;interpretation
;represents the maximum amounts of bomb that each player can put
;the direction of each player should less than or equal maximum

;additional note:
;it is not a fixed constant
;it will raise along with the roundtimer countdowns in one game
;to reduce the probability of tie.

;examples
(define initial-maximum 3)