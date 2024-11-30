;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname code) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; render-cell
; symbol -> Image
(define (render-cell symbol)
  (crop 0 0 CELL-SIZE CELL-SIZE
        (cond
          [(symbol=? symbol 'D) (bitmap "D.png")]
          [(symbol=? symbol 'I) (bitmap "U.png")]
          [(symbol=? symbol 'W) (bitmap "W.png")]
          [(symbol=? symbol 'B) (bitmap "B.png")]
          [(symbol=? symbol 'E) (bitmap "T.png")]

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
  (let loop ((i 0) (n (vector-length layout)) (acc empty-image))
    (if (>= i n)
        acc
        (loop (+ i 1) n (beside acc (render-cell (vector-ref layout i)))))))        


; render-layout
; layout -> Image
(define (render-layout layout)
  (let loop ((i 0) (n (vector-length layout)) (acc empty-image))
    (if (>= i n)
        acc
        (loop (+ i 1) n
              (above acc
                     (render-row (vector-ref layout i)))))))


(define SPACE (square 30 0 "white"))
;render-bar
;roundtimer maximum owner1 owner2 -> Image
(define (render-bar roundtimer maximum owner1 owner2)
  (beside
   (text (string-append "P1: " (number->string owner1) )30 "indigo")
   SPACE
   (text (string-append "roundtimer: " (number->string roundtimer)) 30 "indigo")
   SPACE
   (text (string-append "maximum bomb : " (number->string maximum)) 30 "indigo")
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
        [layout (gamestate-layout gamestate)]
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
(define-struct gamestate [layout bomb player1 player2 roundtimer maximum boom-cor quit?] #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;layout:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;'E represents the cell with exploding bomb

; additional notes:
; -- each game has a random layout
; -- safe area:
; -- fixed position of 'I


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;layout:;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;layout:;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bomb ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bomb ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;data types:
;bomb is one of the following:
; -- '()  ;; no existing boomstate in game
; -- list of bombstate structure

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
;from 5(included)
;to 0(included)

;interpretation:
;represents the countdown of the each bombstate
;5 is the time for bomb just added to game 
;2 is the time for bomb start to explode
;0 is the time for bomb end explode

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;bomb;;;;;;;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player1:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player1:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;data types:
;player1 is a structure
(define-struct player1 [cor direction] #:transparent)
;(make-player1 cor Boolean String)

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player1;;;;;;;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;














;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;data types:
;player2 is a structure
(define-struct player2 [cor direction] #:transparent)
;(make-player2 cor String)

;interpretation is as similar as player1
;just replace all "player1" with "player2"

;example
(define initial-player2 (make-player2
                        (make-cor (- MAX-COLS 1) (- MAX-ROWS 1))
                        "U"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player2;;;;;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player2;;;;;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;














;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;roundtimer:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;roundtimer:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;roundtimer is a Interger in the interval:
;from 120(included)
;to 0(included)

;interpretation
;represents the countdown of one game

;examples;
(define initial-roundtimer 120)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;roundtimer;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;roundtimer;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;maximum;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;maximum;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;maximum is a Interger in the interval:
;from 3(included)
;to 6(included)

;interpretation
;represents the maximum amounts of bomb that each player can put
;the direction of each player should less than or equal maximum

;additional note:
;it is not a fixed constant
;it will raise along with the roundtimer countdowns in one game
;to reduce the probability of tie.

;examples
(define initial-maximum 3)






;;;;;;;;;;;example of the gamestate;;;;;;;;;;;;;;;;;;;;
;(define initial-gamestate
;  initial-layout
;  initial-player1
;  initial-player2
;  initial-roundtimer
;  initial-maximum)




;(define-struct gamestate [layout bomb player1 player2 roundtimer maximum])



;cor layout -> symbol
(define (get-symbol layout cor)
  (if (in-bound? cor)
  (vector-ref
   (vector-ref layout (cor-row cor))
   (cor-column cor))
  'Ill))



;;on-tick
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;no problem;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;cor layout -> Boolean
(define (in-bound? cor)
  (and (<= 0 (cor-row cor) (- MAX-ROWS 1))
       (<= 0 (cor-column cor) (- MAX-COLS 1))))

;cor layout -> Boolean
(define (check-U? layout cor)
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

;row col -> Boolean
;rules for generating 'I
(define (is-U? row col)
  (and (even? row)          
       (odd? col)          
       (> row 0)           
       (< row (- MAX-ROWS 1))  
       (> col 0)            
       (< col (- MAX-COLS 1))))

;row col -> Boolean
(define (is-player1? row col)
  (= 0 (+ row col)))

(define (is-player2? row col)
  (= (+ (- MAX-ROWS 1) (- MAX-COLS 1)) (+ row col)))


;row col -> Boolean
;rules for generating fixed‘W
(define (is-fixed-W? row col)
  (or (< 0 (+ row col) 2) 
      (>= (+ row col) (+ (- MAX-ROWS 1) (- MAX-COLS 1) -2))))

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
       (gamestate-boom-cor gamestate)
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
         [current-cor (player1-cor (gamestate-player1 gamestate))]
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
              [new-symbol (get-symbol layout new-cor)]
              [final-symbol (string->symbol
                             (string-append (symbol->string new-symbol) "1" new-direction))]
              [can-move (move-predicate? layout current-cor new-cor)]
              [updated-layout
               (if can-move
                   (let* (
                          [restored-symbol
                           (cond
                             [(symbol=? current-symbol 'W) 'W]
                             [(or (symbol=? current-symbol 'W1L)
                                  (symbol=? current-symbol 'W1U)
                                  (symbol=? current-symbol 'W1D)
                                  (symbol=? current-symbol 'W1R))
                              'W]
                             [(or (symbol=? current-symbol 'B1L)
                                  (symbol=? current-symbol 'B1U)
                                  (symbol=? current-symbol 'B1D)
                                  (symbol=? current-symbol 'B1R))      
                              'B]
                             [else current-symbol]
                             )]
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
          (gamestate-boom-cor gamestate)
          (gamestate-quit? gamestate)))]
         

     ;;up
      [(string=? ke "up")
       (let* (
              [new-cor (make-cor current-cor-column (- current-cor-row 1))]
              [new-direction "U"]
              [new-symbol (get-symbol layout new-cor)]
              [final-symbol (string->symbol
                             (string-append (symbol->string new-symbol) "1" new-direction))]
              [can-move (move-predicate? layout current-cor new-cor)]
              [updated-layout
               (if can-move
                   (let* ([restored-symbol
                           (cond
                             [(symbol=? current-symbol 'W) 'W]
                             [(or (symbol=? current-symbol 'W1L)
                                  (symbol=? current-symbol 'W1U)
                                  (symbol=? current-symbol 'W1D)
                                  (symbol=? current-symbol 'W1R))
                              'W]
                             [(or (symbol=? current-symbol 'B1L)
                                  (symbol=? current-symbol 'B1U)
                                  (symbol=? current-symbol 'B1D)
                                  (symbol=? current-symbol 'B1R))      
                              'B]
                             [else current-symbol]
                             )]
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
          (gamestate-boom-cor gamestate)
          (gamestate-quit? gamestate)))]

      ;;left
      [(string=? ke "left")
       (let* (
              [new-cor (make-cor (- current-cor-column 1) current-cor-row)]
              [new-direction "L"]
              [new-symbol (get-symbol layout new-cor)]
              [final-symbol (string->symbol
                         (string-append (symbol->string new-symbol) "1" new-direction))]
              [can-move (move-predicate? layout current-cor new-cor)]
              [updated-layout
               (if can-move
                   (let* ([restored-symbol
                           (cond
                             [(symbol=? current-symbol 'W) 'W]
                             [(or (symbol=? current-symbol 'W1L)
                                  (symbol=? current-symbol 'W1U)
                                  (symbol=? current-symbol 'W1D)
                                  (symbol=? current-symbol 'W1R))
                              'W]
                             [(or (symbol=? current-symbol 'B1L)
                                  (symbol=? current-symbol 'B1U)
                                  (symbol=? current-symbol 'B1D)
                                  (symbol=? current-symbol 'B1R))      
                              'B]
                             [else current-symbol]
                             )]
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
          (gamestate-boom-cor gamestate)
          (gamestate-quit? gamestate)))]

  ;;right
      [(string=? ke "right")
       (let* (
              [new-cor (make-cor (+ current-cor-column 1) current-cor-row)]
              [new-direction "R"]
              [new-symbol (get-symbol layout new-cor)]
              [final-symbol (string->symbol
                         (string-append (symbol->string new-symbol) "1" new-direction))]
              [can-move (move-predicate? layout current-cor new-cor)]
              [updated-layout
               (if can-move
                   (let* ([restored-symbol
                           (cond
                             [(symbol=? current-symbol 'W) 'W]
                             [(or (symbol=? current-symbol 'W1L)
                                  (symbol=? current-symbol 'W1U)
                                  (symbol=? current-symbol 'W1D)
                                  (symbol=? current-symbol 'W1R))
                              'W]
                             [(or (symbol=? current-symbol 'B1L)
                                  (symbol=? current-symbol 'B1U)
                                  (symbol=? current-symbol 'B1D)
                                  (symbol=? current-symbol 'B1R))      
                              'B]
                             [else current-symbol]
                             )]
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
          (gamestate-boom-cor gamestate)
          (gamestate-quit? gamestate)))]

      
      ;; put-bomb
      [(string=? ke "a")
       (let* (
              [final-symbol (string->symbol
                             (string-append "B" "1" current-direction))]
              [bomb-list (gamestate-bomb gamestate)]
              [owner1 (count-num (filter
                                  (lambda(bombstate)
                                    (symbol=? (bombstate-owner bombstate) 'P1))
                                  bomb-list))]
              [can-put? (put-predicate? layout current-cor owner1 maximum)]
              [updated-layout
               (if can-put?
                   
                   (convert layout current-cor final-symbol)
                   layout)]
              [updated-bomb-list
               (if can-put?
                    (cons (make-bombstate current-cor 5 'P1) (gamestate-bomb gamestate))
                    (gamestate-bomb gamestate))]
              )
         (make-gamestate
          updated-layout
          updated-bomb-list
          (gamestate-player1 gamestate)
          (gamestate-player2 gamestate)
          (gamestate-roundtimer gamestate)
          (gamestate-maximum gamestate)
          (gamestate-boom-cor gamestate)
          (gamestate-quit? gamestate)))]
          
              
         
         
      ;;
      [else gamestate]))]))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;keyhandler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;stop-when;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
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
        )
    (cond
      [(check-all-died? player1-symbol player2-symbol) #t]
      [(check-player1-died? player1-symbol) #t]
      [(check-player2-died? player2-symbol) #t]
      [else #f]))]))))



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
        )
    (cond
      [(check-all-died? player1-symbol player2-symbol) (tie layout)]
      [(check-player1-died? player1-symbol) (player2-win layout)]
      [(check-player2-died? player2-symbol) (player1-win layout)]))))

    
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
(define (player2-win layout)
  (overlay
   (text "PLAYER2-WIN!"
         100
         "RED")
   (render-layout layout)))

(define (player1-win layout)
  (overlay
   (text "PLAYER1-WIN!"
         100
         "RED")
   (render-layout layout)))

(define (tie layout)
  (overlay
   (text "You Can Get Married!"
         70
         "RED")
   (render-layout layout)))

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
   layout1
   '()
   initial-player1
   initial-player2
   120
   3
  '()
  #f))

(define homepage-state
  (make-gamestate
   homepage
   '()
   #f
   #f
   #f
   #f
   '()
  #f))



;application
(main homepage-state)