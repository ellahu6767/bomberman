;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname public) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/vector)
(require racket/system)
(require racket/base)

(provide (all-defined-out))

;constants
(define MAX-ROWS 11)
(define MAX-COLS 15)

;gamestate is a structure
(define-struct gamestate [layout bomb player1 player2 roundtimer maximum quit?] #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;layout;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                        ;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;layout;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;layout is a
;(Vector of (Vector of Symbol)) 

;interpretation

;the layout of the game is a 2D vector grid composed of same-size-square-shape cells.
;each cell contains a symbol.
;each symbol represents a specific symbol in the game. 


;for each symbol:
;'W represents the walkable cell
;'I represents the indestructible cell
;'D represents the destructible cell
;'B represents the cell with unexploded bomb

;for each three-length symbol
;it represents a player on a specific type of cell, along with the player's name and direction.
;for example, 'W1L represents player1 on the 'W cell with left direction
;'W2U represents player2 on the 'W cell with up direction

;for each two-length symbol
;'E2 'E1 'E0 represents the exploding bomb and its countdown
;for example, 'E2 means one exploding bomb and the exploding will last 3 seconds


;data examples
(define layout-example
  (vector
   (vector 'W1D 'W 'W 'W 'I)
   (vector 'W 'W 'E1 'W 'D)
   (vector 'W 'W 'E2 'W 'D)
   (vector 'W 'B 'W 'W 'D)
   (vector 'W 'W 'W 'W 'W2U)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;bomb;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                     ;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;bomb;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;bomb is one of the following:
; -- '()  ;; no existing boomstate in game
; -- list of bombstate structure
; -- #f when layout is homepage

;interpretation
;represents all of the existing bombstate


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bombstate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;bombstate is a structure
(define-struct bombstate [cor countdown owner] #:transparent)
;interpretation:

;;;;;;;;;;;;;;;;;;;;;;;;;;;; countdown ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;countdown is an Integer in the interval
;from 3(included)
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
;column is a nonnegative Integer from 0 to (- MAX-COLUMNS 1) , represents the location of column
;row is a nonnegative Integer from 0 to (- MAX-ROWS 1), represents the location of row

;data examples
(define exmaple-cor (make-cor 2 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; owner ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;owner is one of the following Symbol
;--'P1 represents this bomb is put by player1
;--'P2 represents this bomb is put by player2


;data examples of bomb
(define initial-bomb '())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                        ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;data types:
;player1 is one of the following:
(define-struct player1 [cor direction] #:transparent)
;-- (make-player1 cor String)
;-- #f when layout is homepage

;interpretation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;cor:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;cor represents the location of player1 in the map
;it is a structure as explained previously(in bombstate part).



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;direction:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;direction is one of the following String:
;-- "U" represents up direction of player
;-- "L" represents left direction of player
;-- "R" represents right direction of player
;-- "D" represents down direction of player



;data example
(define initial-player1 (make-player1
                        (make-cor 0 0)
                        "D"))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                        ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;data types:
;player2 is one of the following:
(define-struct player2 [cor direction] #:transparent)
;-- (make-player2 cor String)
;-- #f when layout is homepage

;interpretation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;cor:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;cor represents the location of player2 in the map
;it is a structure as explained previously(in bombstate part).



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;direction:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;direction is one of the following String:
;-- "U" represents up direction of player
;-- "L" represents left direction of player
;-- "R" represents right direction of player
;-- "D" represents down direction of player


;data example
(define initial-player2 (make-player2
                        (make-cor (- MAX-COLS 1) (- MAX-ROWS 1))
                        "U"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;roundtimer;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;roundtimer;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;roundtimer is one of the following:
;--an Integer in the interval:
;from 120(included) to 0(included)

;-- #f when layout is homepage

;interpretation
;represents the countdown of one game

;examples;
(define initial-roundtimer 120)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;maximum;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;maximum;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;maximum is one of the following:
;--an Integer in the interval:
;from 3(included) to 6(included)

;--#f when layout is homepage

;interpretation
;represents the maximum amounts of bomb that each player can put

;examples
(define initial-maximum 3)


;Row Col -> Boolean
;rules for generating 'I in random-layout
(define (is-I? row col)
  (and (even? row)          
       (odd? col)          
       (< 0 row (- MAX-ROWS 1))                   
       (< 0 col (- MAX-COLS 1))))

;Row Col -> Boolean
;rules for generating 'W1D in random-layout
(define (is-player1? row col)
  (= 0 (+ row col)))            

;Row Col -> Boolean
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
    [(is-HU? row col) 'B]
    [else 'W]))

;row col -> symbol
;rules for generate random-layout
(define (random-layout-rule row col)
  (cond
    [(is-player1? row col) 'W1D] ;generate the player1 at the top-left corner
    [(is-player2? row col) 'W2U] ;generate the player2 at the top-right corner
    [(is-I? row col) 'I] ;generate the fixed indestructible cell
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

;row acc rule-fn -> vector of vector of symbol
;generate layout
(define (generate-layout row acc rule-fn)
  (if (= row MAX-ROWS) ;if all rows come the end
      (list->vector (reverse acc))  ;tranform the list to vector
      (generate-layout ;else
       (+ row 1) ;change to the next row
       (cons (generate-row row 0 '() rule-fn) acc) ;column starts from 0, acc starts from '()
       rule-fn)))


;homepage vector and random-layout vector
(define homepage (generate-layout 0 '() homepage-rule))
(define random-layout (generate-layout 0 '() random-layout-rule))

;initial gamestate
(define initial-state
  (make-gamestate
   random-layout
   '()
   initial-player1
   initial-player2
   initial-roundtimer
   initial-maximum
  #f))

;homepage gamestate
(define homepage-state
  (make-gamestate
   homepage
   '()
   #f
   #f
   #f
   #f
  #f))


;get-symbol:

;input/output:
;layout cor -> symbol

;purpose statement:
;get symbol according to the given layout and cor
(define (get-symbol layout cor)
  (if (in-bound? cor layout)
  (vector-ref
   (vector-ref layout (cor-row cor)) ;the row number
   (cor-column cor)) ;the column number
  'ILEGAL)) ;else no


;in-bound?:

;input/output:
;cor layout -> Boolean

;purpose statement:
;differentiate if the cor is in the valid range of the layout
;if valid return #true
;otherwise return #false
(define (in-bound? cor layout)
  (and (<= 0 (cor-row cor) (- (vector-length layout) 1))     
       (<= 0 (cor-column cor) (- (vector-length (vector-ref layout 0)) 1))))


;count-num
(define (count-num list-of-bomb)
  (cond
    [(empty? list-of-bomb) 0]
    [else
     (add1 (count-num (rest list-of-bomb)))]))


;convert:
;data types:
;cor is one of the following:
;-- a single Cor structure
;-- a list of Cor structure

;input/output:
;layout Cor Symbol -> layout

;purpose statement:
;change the layout according to the given layout Cor and Symbol
(define (convert layout cor symbol)
  (cond
    [(empty? cor) layout]
    [(list? cor)
    (convert
     (convert layout (first cor) symbol) ;convert every element of the coordinate
     (rest cor)
    symbol)]
    [else
     (begin
       (vector-set!
        (vector-ref layout
                    (cor-row cor)) ;the row of layout
        (cor-column cor) ;the column of this row: a specific symbol
        symbol) ;convert the cor symbol on the layout into this symbol
       layout)])) ;return layout




;constant definitions
(define SPACE (square 30 0 "white"))
(define CELL-SIZE (image-width (bitmap "decorations/W.png"))) 

(define player1-image-D (bitmap "decorations/P1D.png"))
(define player1-image-U (bitmap "decorations/P1U.png"))
(define player1-image-L (bitmap "decorations/P1L.png"))
(define player1-image-R (bitmap "decorations/P1R.png"))

(define player2-image-D (bitmap "decorations/P2D.png"))
(define player2-image-U (bitmap "decorations/P2U.png"))
(define player2-image-L (bitmap "decorations/P2L.png"))
(define player2-image-R (bitmap "decorations/P2R.png"))