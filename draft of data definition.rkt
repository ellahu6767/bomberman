;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |draft of data definition|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;;;;;this is a draft of the data definition that work for the basic functions of this game

(define-struct game-state [map bomb player1 player2])
;map is one of the following:
; -- map1
; -- map2
; -- map3 (if we can)
; -- #false

; map1,map2,map3 are (vector(vector(char)))
; map1 = random of 11*15 axiom (finished)
; map2 = easting egg of creator name shape (we can do)
; map3 = the biggest map that created by l-system arithametic (if time permit)
;before choosing a certain map, map = #false, render the starting picture/choosing picture

;player1,player2 is a sturcture
(define-struct player1 [position dead?])
(define-struct player2 [position dead?])

;dead is a Boolean
;means the state of the players

;bomb is one of the following:
(define-struct bomb [maximum list<position>])
;-- #false

;maximum is a Number which can changed
;represents the amounts of bomb that players could use

;position reprents the poistion of the elements
;position is not Posn, its a new defined structure
(define-struct position [row column])
;(define-struct position [Number Number])
;represents the position in the map
;examples
;(make-position 1 2)
;represents the position in the 1 row and 2 column of the map
;warning
;it can not be used directly, we need design a function help the structure position
;access the map

;#false means no bomb in the game now

;on-key
;game-state -> game-state
;handle the movement of the players and use of the bomb
;choose "start game" and the map

;on-mouse (optional)
;game-state -> game-state
;choose "start game" and the map

;render
;game-state -> Image
;render the players,bomb and the map


;stop-when
;game-state -> Boolean
;end when one of the player dead

;constant definitions
;including the design of the cell , players and bomb

