;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname on_tick) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/vector)
(require racket/system)
(require racket/base)
(require "public.rkt")

(provide (all-defined-out))

;check-I?:

;Input/Output:
;Cor Layout -> Boolean

;Purpose Statement:
;Check if the Symbol of the given Cor in the given Layout is 'I.
;If it is, returns #t; otherwise, returns #f.
(define (check-I? layout cor)
  (eq? (get-symbol layout cor) 'I))

;check-D?:

;Input/Output:
;Cor Layout -> Boolean

;Purpose Statement:
;Check if the Symbol of the given Cor in the given Layout is 'D.
;If it is, returns #t; otherwise, returns #f.
(define (check-D? layout cor)
  (eq? (get-symbol layout cor) 'D))


;single-boom-range:

;Input/Output:
;BombState Layout -> List<Cor>

;Purpose Statement:
;Call extend-direction function to calculate the Real boom-range of each direction.
;Return the Real boom-range of a single BombState.
(define (single-boom-range bombstate layout)
  (let* (
        [center (bombstate-cor bombstate)]
        [cor-x (cor-column center)]
        [cor-y (cor-row center)]

        [potential-boom-range-except-center
         (list
        ;;up
         (list (make-cor cor-x (- cor-y 1))
               (make-cor cor-x (- cor-y 2)))
        ;;down
         (list (make-cor cor-x (+ cor-y 1))
               (make-cor cor-x (+ cor-y 2)))
        ;;left
         (list (make-cor (- cor-x 1) cor-y)
               (make-cor (- cor-x 2) cor-y))
        ;;right
         (list (make-cor (+ cor-x 1) cor-y)
               (make-cor (+ cor-x 2) cor-y)))]

        [boom-range-except-center
         (map
          (lambda(potential-list)
            (extend-direction potential-list layout))
          potential-boom-range-except-center)]
        )
    (cons center
          (apply append boom-range-except-center))))


;extend-direction:

;Data Types:
;potential-list is a List<Cor>
;Represents the max-boom-range in each direction

;Input/Output:
;List<Cor> Layout -> List<Cor>

;Purpose Statement:
;Extend the explosion range in each direction
;The extending rule is:
;When meeting the first 'D in any direction, stop extending in this direction but add Cor of 'D to the result
;When meeting the first 'I in any direction, directly stop extending in this direction and return the result
(define (extend-direction potential-list layout)
  (cond
    [(or
      (empty? potential-list)
      (not (in-bound? (first potential-list) layout)))
     '()]

    [(check-I? layout (first potential-list)) '()]

    [(check-D? layout (first potential-list))
     (list (first potential-list))]

    [else
     (cons (first potential-list)
           (extend-direction (rest potential-list) layout))]))


;boom-range:

;Input/Output:
;List<BombState> Layout -> List<Cor>

;Purpose Statement:
;Calculate the boom-range of a List<BombState>
(define (boom-range bomb-list layout)
  (apply append
         (map
          (lambda (bombstate)
                (single-boom-range bombstate layout))
              bomb-list)))


;boom:

;Input/Output:
;GameState -> GameState

;Purpose Statement:
;Filter the Countdown=0 List<BombState>
;Calculate the boom-range
;Handle the Chain-Explosion
;To get the final boom-range
;When finishing handling all the exploding bombs, return the GameState with updated List<BombState> and updated Layout
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
             [exploding-list ;The bombs whose Countdown=0 and not in the processed-bombs list
              (filter
               (lambda (bombstate)
                 (and (= (bombstate-countdown bombstate) 0)
                      (not (in-boom-cor? (bombstate-cor bombstate) processed-bombs))))
               current-bomb-list)] 
             [boom-cor (boom-range exploding-list current-layout)] ;Calculate the boom-range of exploding bombs
             [updated-bomb-list (chain-explosion boom-cor current-bomb-list)] ;Handle chain-explosion
                          [new-processed-bombs (append processed-bombs (map bombstate-cor exploding-list))] ;Calculating new processed bombs
             [new-layout
              (cond
                [(and
                  (in-boom-cor? player1-cor boom-cor)
                  (in-boom-cor? player2-cor boom-cor))
                  
                  (convert ;;Kill player2
                   (convert ;;Kill player1
                    (convert current-layout boom-cor 'E1) ;;Change boom-range to 'E1
                   player1-cor 
                   (string->symbol (string-append "E" "1" player1-direction)))
                  player2-cor ;Cor for convert3
                  (string->symbol (string-append "E" "2" player2-direction)))]
               
                [(in-boom-cor? player1-cor boom-cor)
                 (convert ;;Kill player1
                  (convert current-layout boom-cor 'E1) ;;Change boom-range to 'E1
                  player1-cor (string->symbol (string-append "E" "1" player1-direction)))]
                
                [(in-boom-cor? player2-cor boom-cor)
                 (convert ;;Kill player2
                  (convert current-layout boom-cor 'E1) ;;Change boom-range to 'E1
                  player2-cor (string->symbol (string-append "E" "2" player2-direction)))]

                [else (convert current-layout boom-cor 'E1)])] ;;Only change boom-range to 'E1
             
             [new-accumulated-boom-cor (append accumulated-boom-cor boom-cor)]           
             )
        (if (empty? exploding-list) ;No bombs Countdown=0, or all are in processed-bomb list      
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

;Input/Output:
;List<Cor> List<BombState> -> List<BombState>

;Purpose Statement:
;Handle the Chain-Explosion. The rule is:
;If a bomb's Cor matches any Cor in the Initial-Boom-Cor (a List<Cor>)
;and its countdown is not 0
;this bomb's countdown will be set to 0
;Otherwise, the bomb will be unchanged
;Finally, returns the updated List<BombState> after applying the rule to all
;the elements in the given list<Bombstate>
(define (chain-explosion initial-boom-cor bomb-list)
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
              bomb-list))


;cor=?:

;Input/Output:
;Cor Cor -> Boolean

;Purpose Statement:
;Returns #t if the given two Cors have both the same column and row
;Otherwise, returns #f
(define (cor=? cor1 cor2)
  (and (= (cor-column cor1) (cor-column cor2))
       (= (cor-row cor1) (cor-row cor2))))


;in-boom-cor?:

;Input/Output:
;Cor List<Cor> -> Boolean

;Purpose Statement:
;Returns #t if the given Cor is in the given List<Cor>
;Otherwise, returns #f
(define (in-boom-cor? cor cor-list)
  (cond
    [(empty? cor-list) #f]
    [else
     (if
      (cor=? cor (first cor-list))
      #t
      (in-boom-cor? cor (rest cor-list)))]))


;remove-bomb:

;Input/Output:
;List<BombState> -> List<BombState>

;Purpose Statement:
;Remove the BombState which countdown is 0 in the given List<Bombstate>
(define (remove-bomb bomb-list)
  (filter (lambda (bomb)
            (> (bombstate-countdown bomb) 0)) bomb-list))


;updated-bomb:

;Input/Output:
;List<BombState> -> List<BombState>

;Purpose Statement:
;Updates the countdown of each BombState in the list<Bombstate> (sub1 in each tick)
;But the countdown cannot go below 0

(define (updated-bomb bomb-list)
  (map (lambda (bombstate)
         (make-bombstate
          (bombstate-cor bombstate)
          (max 0 (sub1 (bombstate-countdown bombstate)))
          (bombstate-owner bombstate)))
       bomb-list))



;updated-roundtimer:

;Input/Output:
;Number -> Number

;Purpose Statement:
;Decreases the RoundTimer by 1 in each tick,
;but the RoundTimer doesn't go below 0
(define (updated-roundtimer roundtimer)
  (max 0 (sub1 roundtimer))) 


;check-roundtimer?:

;Input/Output:
;Number -> Boolean

;Purpose Statement:
;Returns #t if the RoundTimer is divisible by 30 (except 120 or 0)
;Otherwise, returns #f
(define (check-roundtimer? roundtimer)
  (and
   (not (= roundtimer 120))
   (not (= roundtimer 0))    
   (= (modulo roundtimer 30) 0)))


;add-maximum:

;Input/Output:
;Number -> Number

;Purpose Statement:
;Increases the given Maximum value by 1
;but Maximum cannot exceed 6
(define (add-maximum maximum)
  (min 6 (add1 maximum)))

;update-E-symbols:

;Input/Output:
;Symbol -> Symbol

;Purpose Statement:
;Change the Symbol in sequence 'E1 -> 'E0 -> 'W in each tick
(define (update-E-symbols symbol)
  (cond
    [(equal? symbol 'E1) 'E0]
    [(equal? symbol 'E0) 'W]
    [else symbol]))

;updated-layout:

;Input/Output:
;Layout -> Layout

;Purpose Statement:
;Updates the Layout by applying the update-E-symbols function to every Symbol in the Layout
(define (updated-layout layout)
  (vector-map
   (lambda (row)
     (vector-map update-E-symbols row)) 
   layout))


;check-zero?:

;Input/Output:
;List<BombState> -> Boolean

;Purpose Statement:
;Returns #t if there is any BombState in the List<Bombstate> that has countdown equal to 0
;Otherwise, returns #f
(define (check-zero? bomb-list)
  (cond
    [(empty? bomb-list) #f] ;
    [else
     (if (= (bombstate-countdown (first bomb-list)) 0)
         #t
         (check-zero? (rest bomb-list)))])) 




;timehandler:

;Input/Output
;GameState -> GameState

;Purpose Statement:
;Update Layout ('E1->'E1->'E0->'W)
;Update the RoundTimer and Maximum, and countdown for all BombState
;When there is any bomb that should boom, call the boom function
;to handle explosions
;After handling explosions, remove exploded bombs

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
             (make-gamestate  
              (updated-layout (gamestate-layout gamestate)) ;'E1->'E0->'W
              (updated-bomb (gamestate-bomb gamestate)) ;Decrease the countdown of each BombState
              (gamestate-player1 gamestate)
              (gamestate-player2 gamestate)
              (updated-roundtimer (gamestate-roundtimer gamestate))
              (if (check-roundtimer? (gamestate-roundtimer gamestate))
                  (add-maximum (gamestate-maximum gamestate))
                  (gamestate-maximum gamestate)) ;Check if Maximum should be added
              (gamestate-quit? gamestate))]

            ;;Handle countdown=0 bombs
            [boom-gamestate
             (if (check-zero? (gamestate-bomb updated-gamestate))
                 (boom updated-gamestate)
                 updated-gamestate)]

            ;;Remove processed countdown=0 bombs
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


