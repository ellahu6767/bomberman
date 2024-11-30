;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname to_draw) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require racket/base)

;;to-draw:
;- top-bar
;- player-image
;- layout-image

;;on-key:
;- player-move
;- check-border
;- place-bomb
;- quit

;------------------------the map/layout--------------------------
;the Layout of the game is a 2D vector grid composed of same-size-square-shape cells.
;each cell contains a symbol.
;each symbol represents a specific element in the game. 


;for each symbol:
;'S represents the safe area for players join the game at the start
;'W represents the walkable cell
;'I represents the indestructible cell
;'D represents the destructible cell
;'B represents the cell with unexploded bomb
;'E represents the cell with exploding bomb

; additional notes:
; -- each game has a random layout,there will be a function(generate-layout base)
; to randomizes the symbols 'W in base to create initial-layout
; -- in fact , 'S and 'W are both walkable cells.
; but , in the (generate-layout base) function, which randomizes the symbols 'W 
; 'S cells remain unchanged to provide a fixed safe area for players starting the game.
; -- similar , 'I is also unchangeble in the (generate-layout base) function

;examples:
(define base
  (vector
   (vector 'S 'S 'S 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W)
   (vector 'S 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W)
   (vector 'S 'W 'I 'W 'I 'W 'I 'W 'I 'W 'I 'W 'I 'W 'W)
   (vector 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W)
   (vector 'W 'W 'I 'W 'I 'W 'I 'W 'I 'W 'I 'W 'I 'W 'W)
   (vector 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W)
   (vector 'W 'W 'I 'W 'I 'W 'I 'W 'I 'W 'I 'W 'I 'W 'W)
   (vector 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W)
   (vector 'W 'W 'I 'W 'I 'W 'I 'W 'I 'W 'I 'W 'I 'W 'S)
   (vector 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'S)
   (vector 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'S 'S 'S)))

;(define initial-layout (generate-layout base))
;-----------------your version-----------------------
;;Data Type
;an S is an Image composed of squares.
;born area, walkable, fix

(define S (square 50 'solid 'pink))

;a W is an Image composed of squares.
;walkable area, random

(define W (square 50 'solid 'white))

;a I is an Image composed of squares.
;indestructible cells, fix

(define I (square 50 'solid 'blue))

;a D is an Image composed of squares.
;destructible cells, random

(define D (square 50 'solid 'gray))


;a B is an Image of squares.
;cells with an exploded bomb, depends on the bomb position

(define BOMB (circle 20 'solid 'black))
(define B (overlay BOMB W))

;a E is an Image composed of squares.
;cells with exploding bomb, depends on the bomb position

(define E (square 50 'solid 'orange))

(define (rad n)
  (random n))

(define (c n)
  (cond
    [(= (rad n) 0) 'W]
    [else 'D]))

;define a random format of base 15x15
;take the base and return the image
(define BASE
  (vector
   (vector 'S 'S 'S (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2))
   (vector 'S 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2))
   (vector 'S (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2))
   (vector (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2))
   (vector (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2))
   (vector (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2))
   (vector (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2))
   (vector (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2))
   (vector (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2))
   (vector (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2))
   (vector (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2))
   (vector (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2))
   (vector (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) 'S)
   (vector (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I (c 2) (c 2) 'I 'S)
   (vector (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) (c 2) 'S 'S 'S)))
   
   
(define (out s)
  (cond
    [(symbol=? s 'S) (overlay (square 48 'solid 'pink) (square 50 'solid 'black))]
    [(symbol=? s 'I) (overlay (square 48 'solid 'blue) (square 50 'solid 'black))]
    [(symbol=? s 'D) (overlay (square 48 'solid 'gray) (square 50 'solid 'black))]
    [(symbol=? s 'W) (overlay (square 48 'solid 'white) (square 50 'solid 'black))]))

;(define (layoutl v l)
;  (for ([i (vector-length (vector-ref v l))])
;    (display (out (vector-ref (vector-ref v l) i)))))

;(define (layoutl v l)
;  (beside (for ([i 15]) (out (vector-ref (vector-ref v l) i)))
;          (out (vector-ref (vector-ref v l) 0))))

;(define X (circle 100 'outline 'pink))
;(define Y (display X))

;(define (layoutl v l)
;  (beside (out (vector-ref (vector-ref v l) 0))
;          (out (vector-ref (vector-ref v l) 1))
;          (out (vector-ref (vector-ref v l) 2))
;          (out (vector-ref (vector-ref v l) 3))
;          (out (vector-ref (vector-ref v l) 4))
;          (out (vector-ref (vector-ref v l) 5))
;          (out (vector-ref (vector-ref v l) 6))
;          (out (vector-ref (vector-ref v l) 7))
;          (out (vector-ref (vector-ref v l) 8))
;          (out (vector-ref (vector-ref v l) 9))
;          (out (vector-ref (vector-ref v l) 10))
;          (out (vector-ref (vector-ref v l) 11))
;          (out (vector-ref (vector-ref v l) 12))
;          (out (vector-ref (vector-ref v l) 13))
;          (out (vector-ref (vector-ref v l) 14))))

(define try
  (for/fold ([image (out (vector-ref (vector-ref BASE 0) 14))]) ; Start with out0.0
            ([i (range 13 -1 -1)])               ; Loop 14 times
    (beside (out (vector-ref (vector-ref BASE 0) i)) image ))) ; Add out0.x to out0.0


          
                                         

  

    


;----------------draw player on the map acc. to posn-------------
;;Data Type
;Player is a Structure:
(define-struct player1 [posn dead? bombcount])
;(make-player Posn Dead? Bombcount)
;where:
;- Posn: a Posn;
;- Dead?: a Boolean;
;- Bombcount: a Number;
;a Player at position Posn, which can put Bombcount number of bombs.
;If the player is dead, then Dead? is #true.


