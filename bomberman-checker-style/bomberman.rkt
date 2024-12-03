;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname bomberman) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/base)
(require 2htdp/image)
(require 2htdp/universe)
(require racket/vector)
(require racket/system)


(define MAX-ROWS 11) 
(define MAX-COLS 15)

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
  (or (< 0 (+ row col) 2) 
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
    [(is-player1? row col) 'W1D]
    [(is-player2? row col) 'W2U]
    [(is-U? row col) 'I]
    [(is-fixed-W? row col) 'W]
    [else (if (zero? (random 2)) 'D 'W)]))

;row col acc rule-fn -> vector of symbol
;generate row
(define (generate-row row col acc rule-fn)
  (if (= col MAX-COLS)
      (list->vector (reverse acc))
      (generate-row
       row
       (+ col 1)
       (cons (rule-fn row col) acc)
       rule-fn)))

;row acc rule-fn -> vector of vector of symbol
;generate layout
(define (generate-layout row acc rule-fn)
  (if (= row MAX-ROWS)
      (list->vector (reverse acc))
      (generate-layout
       (+ row 1)
       (cons (generate-row row 0 '() rule-fn) acc)
       rule-fn)))


;homepage and random-layout
(define homepage (generate-layout 0 '() homepage-rule))
(define random-layout (generate-layout 0 '() random-layout-rule))



;constant definitions
(define CELL-SIZE (image-width (bitmap "W.png")))

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
  (let loop (
             [i 0]
             [acc empty-image]
             )
    (if (>= i MAX-COLS)
        acc
        (loop (+ i 1)
              (beside acc (render-cell (vector-ref layout i)))))))        


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


;render
;gamestate -> Image
(define (render gamestate)
  (let (
        [layout (gamestate-layout gamestate)] ;abbreviation: layout=gamestate-layout gamestate
        )
    (cond
      [(equal? layout homepage)
       (above
        homepage-bar
        (render-layout homepage))]
      [else
    
  (let* (
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
;countdown is a Integer in the interval
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
;-- from 120(included)
;-- to 0(included)

;-- #f in homepage

;interpretation
;represents the countdown of one game

;examples;
(define initial-roundtimer 120)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;roundtimer;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;maximum;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;maximum is one of the following:
;maximum is a Interger in the interval:
;from 3(included)
;to 6(included)
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



;cor layout -> symbol
(define (get-symbol layout cor)
  (if (in-bound? cor)
  (vector-ref
   (vector-ref layout (cor-row cor))
   (cor-column cor))
  'ILL))

;cor layout -> Boolean
(define (in-bound? cor)
  (and (<= 0 (cor-row cor) (- MAX-ROWS 1))
       (<= 0 (cor-column cor) (- MAX-COLS 1))))

;cor layout -> Boolean
(define (check-I? layout cor)
  (eq? (get-symbol layout cor) 'I))

;cor layout -> Boolean
(define (check-D? layout cor)
  (eq? (get-symbol layout cor) 'D))

;convert:
;data types
;layout is a vector
;cor is a cor or list of cor
(define (convert layout cor symbol)
  (cond
    [(empty? cor) layout]
    [(list? cor)
    (convert
     (convert layout (first cor) symbol)
     (rest cor)
    symbol)]
    [else
     (begin
       (vector-set!
        (vector-ref layout
                    (cor-row cor))
        (cor-column cor)
        symbol)
       layout)]))

;constant definitions
(define BOOM-MAX-DISTANCE 2)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;boom-range;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;meet'D add 'D cor to boom-range and stop
;meet'I , stop
;reach the boom-max-distance, stop
;otherwise, continue loop
(define (extend-direction direction-fn layout)
  (let loop (
             [n 1]
             [acc '()]
             )
    (cond
      [(> n BOOM-MAX-DISTANCE) acc]
      [else
       (let (
             [new-cor (direction-fn n)]
             )
         (cond
           [(not (in-bound? new-cor)) acc]
           [(check-I? layout new-cor) acc]
           [(check-D? layout new-cor) (cons new-cor acc)]
           [else (loop
                  (add1 n)
                  (cons new-cor acc))]))])))


;boom-range:
(define (single-boom-range bombstate layout)
  (let* (
         [center (bombstate-cor bombstate)]
         
         [directions
          (list
           (lambda (n) (make-cor (cor-column center) (- (cor-row center) n)))
           (lambda (n) (make-cor (cor-column center) (+ (cor-row center) n)))
           (lambda (n) (make-cor (- (cor-column center) n) (cor-row center)))
           (lambda (n) (make-cor (+ (cor-column center) n) (cor-row center))))]

         [boom-range-except-center
          (map
           (lambda(direction-fn)
             (extend-direction direction-fn layout))
               directions)]
         )
    (cons center
          (apply append boom-range-except-center))))

; boom-range: list<bombstate> layout -> list<cor>
(define (boom-range bomb-list layout)
  (apply append
         (map (lambda (bombstate)
                (single-boom-range bombstate layout))
              bomb-list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;boom;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;gamestate -> gamestate 
(define (boom gamestate) 
  (let* (
         [bomb-list (gamestate-bomb gamestate)]
         [layout (gamestate-layout gamestate)]
         [player1-cor (player1-cor (gamestate-player1 gamestate))]
         [player2-cor (player2-cor (gamestate-player2 gamestate))]
         [player1-direction (player1-direction (gamestate-player1 gamestate))]
         [player2-direction (player2-direction (gamestate-player2 gamestate))]
         )
    (let loop (
               [current-bomb-list bomb-list]
               [current-layout layout]
               [processed-bombs '()]
               [accumulated-boom-cor '()] 
               )
      (let* (
             [exploding-list
              (filter
               (lambda (bombstate)
                 (and (= (bombstate-countdown bombstate) 0)
                      (not (in-boom-cor? (bombstate-cor bombstate) processed-bombs))))
               current-bomb-list)]
             [boom-cor (boom-range exploding-list current-layout)]
             [updated-bomb-list (chain-explosion boom-cor current-bomb-list current-layout)]
             [new-processed-bombs (append processed-bombs (map bombstate-cor exploding-list))]

             [new-layout
              (cond
                [(and
                  (in-boom-cor? player1-cor boom-cor)
                  (in-boom-cor? player2-cor boom-cor))
                  
                  (convert ;;kill player2
                   (convert ;;kill player1
                    (convert current-layout boom-cor 'E2) ;;change boom-range to 'E2
                   player1-cor (string->symbol (string-append "E" "1" player1-direction)))
                  
                  player2-cor (string->symbol (string-append "E" "2" player2-direction)))]

                [(in-boom-cor? player1-cor boom-cor)
                 
                 (convert ;;kill player1
                  (convert current-layout boom-cor 'E2) ;;change boom-range to 'E2
                  player1-cor (string->symbol (string-append "E" "1" player1-direction)))]
                
                [(in-boom-cor? player2-cor boom-cor)
                 
                 (convert ;;kill player2
                  (convert current-layout boom-cor 'E2) ;;change boom-range to 'E2
                 player2-cor (string->symbol (string-append "E" "2" player2-direction)))]

                [else (convert current-layout boom-cor 'E2)])] ;;change boom-range to 'E2
             
             [new-accumulated-boom-cor (append accumulated-boom-cor boom-cor)]           
             )
        (if (empty? exploding-list)       
             (make-gamestate
              new-layout
              updated-bomb-list
              (gamestate-player1 gamestate)
              (gamestate-player2 gamestate)
              (gamestate-roundtimer gamestate)
              (gamestate-maximum gamestate)
              (gamestate-quit? gamestate))
            (loop updated-bomb-list
                  new-layout
                  new-processed-bombs
                  new-accumulated-boom-cor))))))




;chain-explosion:
;list-of-cor bomb-list layout -> bomb-list
(define (chain-explosion initial-boom-cor bomb-list layout)
    (map
     (lambda (bombstate)
             (if 
               (and
                (not (= (bombstate-countdown bombstate) 0))
                (in-boom-cor? (bombstate-cor bombstate) initial-boom-cor))

                (make-bombstate (bombstate-cor bombstate)
                                0
                                (bombstate-owner bombstate))
             
                bombstate))
     
              bomb-list)
    )

;cor cor -> Boolean
(define (cor=? cor1 cor2)
  (and (= (cor-column cor1) (cor-column cor2))
       (= (cor-row cor1) (cor-row cor2))))
  
;cor list<cor> -> Boolean
(define (in-boom-cor? cor cor-list)
  (cond
    [(empty? cor-list) #f]
    [else
     (if
      (cor=? cor (first cor-list))
      #t
      (in-boom-cor? cor (rest cor-list)))]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;boom;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      
;list<bombstate> -> list<bombstate>
;convert new bomb list with every countdown > 0
(define (remove-bomb bomb-list)
  (let* (
         [removed-bombs (filter (lambda (bomb)
                                   (<= (bombstate-countdown bomb) 0)) bomb-list)]
         [remaining-bombs (filter (lambda (bomb)
                                    (> (bombstate-countdown bomb) 0)) bomb-list)]
         )
    remaining-bombs))


;Number -> Number
(define (updated-roundtimer roundtimer)
  (max 0 (sub1 roundtimer)))

;Number -> Boolean
(define (check-roundtimer? roundtimer)
  (and
   (not (= roundtimer 120))
   (not (= roundtimer 0))    
   (= (modulo roundtimer 30) 0)))

;Number -> Number
(define (add-maximum maximum)
  (min 6 (add1 maximum)))

;bomb -> bomb
(define (updated-bomb bomb)
  (map (lambda (bombstate)
         (make-bombstate
          (bombstate-cor bombstate)
          (max 0 (sub1 (bombstate-countdown bombstate)))
          (bombstate-owner bombstate)))
       bomb))
 

;list<bomb> -> Boolean
(define (check-zero? lob)
  (cond
    [(empty? lob) #f]
    [else
     (if
     (= (bombstate-countdown (first lob)) 0)
     #t
     (check-zero? (rest lob)))]))


;update-T-symbols
;symbol -> symbol
(define (update-T-symbols symbol)
  (cond
    [(equal? symbol 'E2) 'E1]
    [(equal? symbol 'E1) 'E0]
    [(equal? symbol 'E0) 'W]
    [else symbol]))

           
;layout -> layout
(define (updated-layout layout)
  (vector-map
   (lambda(row)
           (vector-map update-T-symbols row))
   layout))

         
;gamestate -> gamestate
(define (timehandler gamestate)
  (let (
        [layout (gamestate-layout gamestate)]
        )
    (cond
      [(equal? layout homepage)
       gamestate]
      [else
       
  (let*
      (
       [updated-gamestate
        (make-gamestate  (updated-layout (gamestate-layout gamestate))
                         (updated-bomb (gamestate-bomb gamestate))
                         (gamestate-player1 gamestate)
                         (gamestate-player2 gamestate)
                         (updated-roundtimer (gamestate-roundtimer gamestate))
                         (if (check-roundtimer? (gamestate-roundtimer gamestate))
                                                (add-maximum (gamestate-maximum gamestate))
                                                (gamestate-maximum gamestate))
                         (gamestate-quit? gamestate))]
     
       
       ;;update game state (update timer for roundtimer and bomb-countwdown)

       ;;handle countdown-zero bomb
       [boom-gamestate
        (if (check-zero? (gamestate-bomb updated-gamestate))
            (boom updated-gamestate)
            updated-gamestate)]

       ;;remove countdown=0 bomb
       [updated-bomb-list (gamestate-bomb boom-gamestate)]
       [final-bomb-list
        (remove-bomb updated-bomb-list)]
       [new-gamestate
        (make-gamestate
         (gamestate-layout boom-gamestate)
         final-bomb-list
         (gamestate-player1 boom-gamestate)
         (gamestate-player2 boom-gamestate)
         (gamestate-roundtimer boom-gamestate)
         (gamestate-maximum boom-gamestate)
         (gamestate-quit? boom-gamestate))]
       )
    new-gamestate)])))
       
   
    

       
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;keyhandler
;gamestate ke -> gamestate

;; move-predicate?: gamestate cor -> Boolean

(define (move-predicate? layout new-cor)
  (let (
        [new-symbol (get-symbol layout new-cor)]
        )
  (and (in-bound? new-cor)
       (or
        (symbol=? new-symbol 'W)
        (= (string-length (symbol->string new-symbol)) 2)))))

(define (put-predicate? layout current-cor owner1 maximum)
  (let (
        [current-symbol (get-symbol layout current-cor)]
        )
    (and (< owner1 maximum)
         (= (string-length (symbol->string current-symbol)) 3))))


;;keyhandler:
;;gamestate key -> gamestate
(define (keyhandler gamestate ke)
  (if (string=? ke "q")
      (make-gamestate
       (gamestate-layout gamestate)
       (gamestate-bomb gamestate)
       (gamestate-player1 gamestate)
       (gamestate-player2 gamestate)
       (gamestate-roundtimer gamestate)
       (gamestate-maximum gamestate)
       #t)
  (let (
        [layout (gamestate-layout gamestate)]
        )
    (cond
      [(equal? layout homepage)
       (if (string=? ke " ")
           initial-state
           gamestate)]
      [else
  (let* (
         [layout (gamestate-layout gamestate)]
         [current-cor (player1-cor (gamestate-player1 gamestate))] ;the cor where player 1 is
         [current-symbol (get-symbol layout current-cor)]
         [current-direction (player1-direction (gamestate-player1 gamestate))]
         [current-cor-column (cor-column current-cor)]
         [current-cor-row (cor-row current-cor)]
         [maximum (gamestate-maximum gamestate)]
         )
    
    (cond
      
      ;;down
      [(string=? ke "down")
       (let* (
              [new-cor (make-cor current-cor-column (+ current-cor-row 1))]
              [new-direction "D"]
              ;the direction is still old
              [new-symbol (string->symbol (string-ith (symbol->string (get-symbol layout new-cor)) 0))]
              ;the first letter of the symbol of the new-cor
              [final-symbol (string->symbol
                             (string-append (symbol->string new-symbol) "1" new-direction))]
              ;new-symbol 1 newdirection
              [can-move (move-predicate? layout new-cor)]
              ;is the next direction movable?
              [updated-layout
               (if can-move
                   (let* (
                          [restored-symbol ;old cor without the player
                           (if (= (string-length (symbol->string current-symbol)) 3)
                               (string->symbol (string-ith (symbol->string current-symbol) 0))
                               ;take the first symbol of the currect-cor, where the player 1 is
                               current-symbol)]
                          )
                                                      
                     (convert
                      (convert layout new-cor final-symbol)
                      current-cor restored-symbol)) ;restored-symbol on the layout

                   (convert layout current-cor ;else
                            (string->symbol
                             (string-append
                              (substring (symbol->string current-symbol) 0 1) ;currect-cor, unchanged
                              "1" ;player1, unchanged                                     
                              new-direction))))] ;new-direction, changed                     

              )               
         (make-gamestate
          updated-layout
          (gamestate-bomb gamestate)
          (make-player1
           (if can-move
               new-cor
               current-cor)
           new-direction) 
          (gamestate-player2 gamestate)
          (gamestate-roundtimer gamestate)
          (gamestate-maximum gamestate)
          (gamestate-quit? gamestate)))]
         

     ;;up
      [(string=? ke "up")
       (let* (
              [new-cor (make-cor current-cor-column (- current-cor-row 1))]
              [new-direction "U"]
              [new-symbol (string->symbol (string-ith (symbol->string (get-symbol layout new-cor)) 0))]
              [final-symbol (string->symbol
                             (string-append (symbol->string new-symbol) "1" new-direction))]
              [can-move (move-predicate? layout new-cor)]
              [updated-layout
               (if can-move
                   (let* ([restored-symbol
                           (if (= (string-length (symbol->string current-symbol)) 3)
                               (string->symbol (string-ith (symbol->string current-symbol) 0))
                               current-symbol)]
                          )
                     (convert
                      (convert layout new-cor final-symbol)
                      current-cor restored-symbol))
                   (convert layout current-cor
                            (string->symbol
                             (string-append
                              (substring (symbol->string current-symbol) 0 1)
                              "1"
                              new-direction))))]                       
          )               
     (make-gamestate
          updated-layout
          (gamestate-bomb gamestate)
          (make-player1
           (if can-move
               new-cor
               current-cor)
           new-direction) 
          (gamestate-player2 gamestate)
          (gamestate-roundtimer gamestate)
          (gamestate-maximum gamestate)
          (gamestate-quit? gamestate)))]

      ;;left
      [(string=? ke "left")
       (let* (
              [new-cor (make-cor (- current-cor-column 1) current-cor-row)]
              [new-direction "L"]
              [new-symbol (string->symbol (string-ith (symbol->string (get-symbol layout new-cor)) 0))]
              [final-symbol (string->symbol
                         (string-append (symbol->string new-symbol) "1" new-direction))]
              [can-move (move-predicate? layout new-cor)]
              [updated-layout
               (if can-move
                   (let* ([restored-symbol
                           (if (= (string-length (symbol->string current-symbol)) 3)
                               (string->symbol (string-ith (symbol->string current-symbol) 0))
                               current-symbol)]
                          )
                     
                     (convert
                      (convert layout new-cor final-symbol)
                      current-cor restored-symbol))
                   (convert layout current-cor
                            (string->symbol
                             (string-append
                              (substring (symbol->string current-symbol) 0 1)
                              "1"
                              new-direction))))]                       
          )               
     (make-gamestate
          updated-layout
          (gamestate-bomb gamestate)
          (make-player1
           (if can-move
               new-cor
               current-cor)
           new-direction) 
          (gamestate-player2 gamestate)
          (gamestate-roundtimer gamestate)
          (gamestate-maximum gamestate)
          (gamestate-quit? gamestate)))]

  ;;right
      [(string=? ke "right")
       (let* (
              [new-cor (make-cor (+ current-cor-column 1) current-cor-row)]
              [new-direction "R"]
              [new-symbol (string->symbol (string-ith (symbol->string (get-symbol layout new-cor)) 0))]
              [final-symbol (string->symbol
                         (string-append (symbol->string new-symbol) "1" new-direction))]
              [can-move (move-predicate? layout new-cor)]
              [updated-layout
               (if can-move
                   (let* ([restored-symbol
                           (if (= (string-length (symbol->string current-symbol)) 3)
                               (string->symbol (string-ith (symbol->string current-symbol) 0))
                               current-symbol)]

                          )
                     
                     (convert
                      (convert layout new-cor final-symbol)
                      current-cor restored-symbol))
                   (convert layout current-cor
                            (string->symbol
                             (string-append
                              (substring (symbol->string current-symbol) 0 1)
                              "1"
                              new-direction))))]                       
          )               
     (make-gamestate
          updated-layout
          (gamestate-bomb gamestate)
          (make-player1
           (if can-move
               new-cor
               current-cor)
           new-direction) 
          (gamestate-player2 gamestate)
          (gamestate-roundtimer gamestate)
          (gamestate-maximum gamestate)
          (gamestate-quit? gamestate)))]

      
      ;; put-bomb
      [(string=? ke "a")
       (let* (
              [final-symbol (string->symbol
                             (string-append "B" "1" current-direction))]
              [bomb-list (gamestate-bomb gamestate)]
              [owner1 (count-num (filter ;how many bombs belong to player1
                                  (lambda(bombstate)
                                    (symbol=? (bombstate-owner bombstate) 'P1)) ;the bombs those belong to player1
                                  bomb-list))]
              [can-put? (put-predicate? layout current-cor owner1 maximum)] ;is the cor putable?
              [updated-layout
               (if can-put?
                   
                   (convert layout current-cor final-symbol)
                   layout)] ;if yes put the bomb there
              [updated-bomb-list
               (if can-put?
                    (cons (make-bombstate current-cor 3 'P1) (gamestate-bomb gamestate))
                    ;add the bomb into List<Bomb>, set the count-down to 3
                    (gamestate-bomb gamestate))]
              )
         (make-gamestate
          updated-layout
          updated-bomb-list
          (gamestate-player1 gamestate)
          (gamestate-player2 gamestate)
          (gamestate-roundtimer gamestate)
          (gamestate-maximum gamestate)
          (gamestate-quit? gamestate)))]
          
              
         
         
      ;;
      [else gamestate]))]))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;keyhandler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;stop-when;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;gamestate -> Boolean
;in homepage or random-layout,press esc,return#t

;in random-layout, one of the following situation,return #t
;players all died
;or player1 died
;or player2 died
;or time finish
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
        [layout (gamestate-layout gamestate)]
        [player1-cor (player1-cor (gamestate-player1 gamestate))]
        [player2-cor (player2-cor (gamestate-player2 gamestate))]
        [player1-symbol (get-symbol layout player1-cor)]
        [player2-symbol (get-symbol layout player2-cor)]
        [time-finish? (= 0 (gamestate-roundtimer gamestate))]
        )
    (or
      (check-all-died? player1-symbol player2-symbol) ;#t if both are dead
      (check-player1-died? player1-symbol) ;#t if p1 is dead
      (check-player2-died? player2-symbol) ;#t if p2 is dead
      time-finish?))])))) ;#t if time is finished



;gamestate-> Image
;quit? -> quit-image
;check-all-died -> tie Image
;check-player1-died -> player2 win Image
;check-player2-died -> player1 win Image
;time-finish -> tie Image
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


;player2-win
(define (player2-win gamestate)
  (overlay
   (text "PLAYER2-WIN!"
         100
         "RED")
   (render gamestate)))

(define (player1-win gamestate)
  (overlay
   (text "PLAYER1-WIN!"
         100
         "RED")
   (render gamestate)))

(define (tie gamestate)
  (overlay
   (text "You Can Get Married!"
         70
         "RED")
   (render gamestate)))

;; layout -> Boolean
(define (check-player1-died? symbol)
  (or
   (symbol=? symbol 'E1L)
   (symbol=? symbol 'E1R)
   (symbol=? symbol 'E1U)
   (symbol=? symbol 'E1D)))

(define (check-player2-died? symbol)
  (or
   (symbol=? symbol 'E2L)
   (symbol=? symbol 'E2R)
   (symbol=? symbol 'E2U)
   (symbol=? symbol 'E2D)))  

(define (check-all-died? s1 s2)
  (and
   (or
    (symbol=? s1 'E1L)
    (symbol=? s1 'E1R)
    (symbol=? s1 'E1U)
    (symbol=? s1 'E1D))
   (or
    (symbol=? s2 'E2L)
    (symbol=? s2 'E2R)
    (symbol=? s2 'E2U)
    (symbol=? s2 'E2D))))  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;stop-when;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;main function
(define (main gamestate)
  (big-bang gamestate
    [to-draw render]
    [on-key keyhandler]
    [on-tick timehandler 0.5]
    [stop-when end?
               final]))
    
  
     

             
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



;application
(system "open ~/Desktop/backup/background.mp3")
(main homepage-state)
















