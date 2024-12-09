;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname public) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/vector)
(require racket/system)
(require racket/base)

(provide (all-defined-out))

; Constant Definitions
(define MAX-ROWS 11)
(define MAX-COLS 15)

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

; Gamestate Is A Structure
(define-struct gamestate [layout bomb player1 player2 roundtimer maximum quit?] #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Layout;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                        ;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Layout;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Types:
; Layout Is A
; Vector<Vector<Symbol>>

; Interpretation
; The Layout Of The Game Is A 2D Vector Grid Composed Of Same-Size-Square-Shape Cells
; Each Cell Contains A Symbol
; Each Symbol Represents A Specific Symbol In The Game

; For Each One-Letter Symbol:
; 'W Represents The Walkable Cell
; 'I Represents The Indestructible Cell
; 'D Represents The Destructible Cell
; 'B Represents The Cell With Unexploded Bomb

; For Each Three-Length Symbol
; It Represents A Player On A Specific Type Of Cell, Along With The Player's Name And Direction
; For Example, 'W1L Represents Player1 On The 'W Cell With Left Direction
; 'W2U Represents Player2 On The 'W Cell With Up Direction

; For Each Two-Length Symbol
; 'E1 'E0 Represents The Exploding Bomb And Its Countdown
; For Example, 'E1 Means One Exploding Bomb And The Exploding Will Last 2 Seconds

; Data Examples
(define layout-example
  (vector
   (vector 'W1D 'W 'W 'W 'I)
   (vector 'W 'W 'E1 'W 'D)
   (vector 'W 'W 'E2 'W 'D)
   (vector 'W 'B 'W 'W 'D)
   (vector 'W 'W 'W 'W 'W2U)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Bomb;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                     ;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Bomb;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Types:
; Bomb Is One Of The Following:
; -- '()  No Existing Bombstate In Game
; -- List<Bombstate>
; -- #f When Layout Is Homepage

; Interpretation
; Represents All Of The Existing Bombstate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Bombstate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Types:
; Bombstate Is A Structure
(define-struct bombstate [cor countdown owner] #:transparent)

; Interpretation:

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Countdown ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Types:
; Countdown Is An Integer In The Interval
; From 3(Included)
; To 0(Included)

; Interpretation
; Represents The Countdown Of Each Bombstate
; 3 Is The Time For Bomb Just Added To Game 
; 0 Is The Time For Bomb Start To Explode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Types:
; Cor Is A Structure
(define-struct cor [column row] #:transparent)
; (make-cor Integer Integer)

; Interpretation
; Represents The Cell Location In The Game
; Column Is A Nonnegative Integer From 0(included) To (- MAX-COLUMNS 1) (included)
; Which Represents The Location Of Column
; Row Is A Nonnegative Integer From 0(included) To (- MAX-ROWS 1) (included)
; Which Represents The Location Of Row

; Data Examples
(define example-cor (make-cor 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Owner ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Types:
; Owner Is One Of The Following Symbol
; -- 'P1 Represents This Bomb Is Put By Player1
; -- 'P2 Represents This Bomb Is Put By Player2

; Data Examples Of Bomb
(define initial-bomb '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Player1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                        ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Player1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Types:
; Player1 Is One Of The Following:
(define-struct player1 [cor direction] #:transparent)
; -- (make-player1 Cor String)
; -- #f When Layout Is Homepage

; Interpretation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Cor;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Cor Represents The Location Of Player1 In The Map
; It Is A Structure As Explained Previously(In Bombstate Part)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Direction;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Direction Is One Of The Following String:
; -- "U" Represents Up Direction Of Player
; -- "L" Represents Left Direction Of Player
; -- "R" Represents Right Direction Of Player
; -- "D" Represents Down Direction Of Player

; Data Example
(define initial-player1 (make-player1
                        (make-cor 0 0)
                        "D"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Player2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                        ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Player2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Types:
; Player2 Is One Of The Following:
(define-struct player2 [cor direction] #:transparent)
; -- (make-player2 Cor String)
; -- #f When Layout Is Homepage

; Interpretation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Cor;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Cor Represents The Location Of Player2 In The Map
; It Is A Structure As Explained Previously(In Bombstate Part)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Direction;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Direction Is One Of The Following String:
; -- "U" Represents Up Direction Of Player
; -- "L" Represents Left Direction Of Player
; -- "R" Represents Right Direction Of Player
; -- "D" Represents Down Direction Of Player

; Data Example
(define initial-player2 (make-player2
                        (make-cor (- MAX-COLS 1) (- MAX-ROWS 1))
                        "U"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Roundtimer;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Roundtimer;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Types:
; Roundtimer Is One Of The Following:
; -- An Integer In The Interval:
; From 120(Included) To 0(Included)

; -- #f When Layout Is Homepage

; Interpretation
; Represents The Countdown Of One Game

; Examples
(define initial-roundtimer 120)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Maximum;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Maximum;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Data types:
;Maximum Is One Of The Following:
;--An Integer In The Interval:
;From 3 (included) to 6 (included)
;--#f When Layout is Homepage

;Interpretation
;Represents The Maximum Amounts Of Bomb That Each Player Can Put

;Data Examples
(define initial-maximum 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Quit?;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Quit?;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Data types:
;Quit? Is a Boolean

;Interpretation
;Represents If The Game Is Ended By Players
;If Players press Key "q" , Quit? Will Be #t
;Otherwise #f


;is-I? :

;Input/Output:
;row col -> Boolean

;Purpose Statement:
;Rules for generating 'I in random-layout
(define (is-I? row col)
  (and (even? row)          
       (odd? col)          
       (< 0 row (- MAX-ROWS 1))                   
       (< 0 col (- MAX-COLS 1))))

;is-player1? :

;Input/Output:
;row col -> Boolean

;Purpose Statement:
;Rules for generating player1 in random-layout
(define (is-player1? row col)
  (= 0 (+ row col)))            

;is-player2? :

;Input/Output:
;row col -> Boolean

;Purpose Statement:
;Rules for generating player2 in random-layout
(define (is-player2? row col)
  (= (+ (- MAX-ROWS 1) (- MAX-COLS 1)) (+ row col)))

;is-fixed-W? :

;Input/Output:
;row col -> Boolean

;Purpose Statement:
;Rules for generating fixed-W in random-layout
(define (is-fixed-W? row col)
  (or (and
       (< 0 (+ row col))
       (<= (+ row col) 2)) 
      (>= (+ row col) (+ (- MAX-ROWS 1) (- MAX-COLS 1) -2))))

;is-YU? :

;Input/Output:
;row col -> Boolean

;Purpose Statement:
;Rules for generating Shape YU in homepage
(define (is-YU? row col)
  (or
   ;; Y shape
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
   ;; U shape
   (and
    (= col 8)
    (<= 0 row 4))
   (and
    (= col 12)
    (<= 0 row 4))
   (and
    (= row 4)
    (<= 8 col 12))))

;is-HU?

;Input/Output:
;row col -> Symbol

;Purpose Statement
;Rules for generating shape HU in homepage
(define (is-HU? row col)
  (or
   ;; U shape
   (and
    (= col 8)
    (<= 6 row 10))
   (and
    (= col 12)
    (<= 6 row 10))
   (and
    (= row 10)
    (<= 8 col 12))
   ;; H shape
   (and
    (= col 0)
    (<= 6 row 10))
   (and
    (= col 4)
    (<= 6 row 10))
   (and
    (= row 8)
    (<= 0 col 4))))

;homepage-rule

;Input/Output:
;row col -> Symbol

;Purpose Statement
;Rules for generating homepage
(define (homepage-rule row col)
  (cond
    [(is-YU? row col) 'D]
    [(is-HU? row col) 'B]
    [else 'W]))

;random-layout-rule

;Input/Output:
;row col -> Symbol

;Purpose Statement
;Rules for generating random-layout
(define (random-layout-rule row col)
  (cond
    [(is-player1? row col) 'W1D] ; Generate player1 at the top-left corner
    [(is-player2? row col) 'W2U] ; Generate player2 at the top-right corner
    [(is-I? row col) 'I] ; Generate the fixed indestructible cell
    [(is-fixed-W? row col) 'W] ; Generate the fixed safe starting area
    [else (if (zero? (random 2)) 'D 'W)])) ; Generate others cell random with 'D or 'W

;generate-layout:

;Input/Output:
;row Acc Rule-Fn -> Vector<Vector<Symbol>>

;Purpose Statement:
;Generate Layout according the given Rule-Function 
(define (generate-layout row acc rule-fn)
  (if (= row MAX-ROWS)
      (list->vector (reverse acc)) 
      (let (
            [current-row 
             (local [(define (generate-row col acc)
                       (if (= col MAX-COLS)
                           (list->vector (reverse acc)) 
                           (generate-row (+ col 1) (cons (rule-fn row col) acc))))]
               (generate-row 0 '()))])
        (generate-layout 
         (+ row 1) 
         (cons current-row acc) 
         rule-fn))))

; Homepage and random-layout
(define homepage (generate-layout 0 '() homepage-rule))
(define random-layout (generate-layout 0 '() random-layout-rule))

;Data examples of Gamestate

;Initial Gamestate
(define initial-state
  (make-gamestate
   random-layout
   initial-bomb
   initial-player1
   initial-player2
   initial-roundtimer
   initial-maximum
   #f))

;Homepage Gamestate
(define homepage-state
  (make-gamestate
   homepage
   #f
   #f
   #f
   #f
   #f
  #f))


;get-symbol:

;Input/Output:
;Layout Cor -> Symbol

;Purpose Statement:
;Get Symbol according to the given Layout and Cor
(define (get-symbol layout cor)
  (if (in-bound? cor layout)
  (vector-ref
   (vector-ref layout (cor-row cor)) ;the row number
   (cor-column cor)) ;the column number
  'ILEGAL)) ;else no


;in-bound?:

;Input/Output:
;Cor Layout -> Boolean

;Purpose Statement:
;Differentiate if the Cor is in the valid range of the layout
;If valid return #true
;Otherwise return #false
(define (in-bound? cor layout)
  (and (<= 0 (cor-row cor) (- (vector-length layout) 1))     
       (<= 0 (cor-column cor) (- (vector-length (vector-ref layout 0)) 1))))


;count-num:

;Input/Output:
;List<Bombstate> -> Integer

;Purpose Statement:
;Count the amounts of bombs in the given List<Bombstate>
(define (count-num list-of-bomb)
  (cond
    [(empty? list-of-bomb) 0]
    [else
     (add1 (count-num (rest list-of-bomb)))]))


;convert:

;Data Types:
;cor is one of the following:
;-- a single Cor Structure
;-- a List<Cor>

;Input/Output:
;Layout Cor Symbol -> layout

;Purpose Statement:
;Change the layout according to the given layout Cor and Symbol
(define (convert layout cor symbol)
  (cond
    [(empty? cor) layout]

    ;;when cor is a List<Cor>
    [(list? cor)
    (convert
     (convert layout (first cor) symbol) ;Convert every element of the coordinate
     (rest cor)
    symbol)]

    ;;when cor is a single Cor Structure
    [else
     (begin
       (vector-set!
        (vector-ref layout
                    (cor-row cor)) 
        (cor-column cor) 
        symbol) 
       layout)])) 



