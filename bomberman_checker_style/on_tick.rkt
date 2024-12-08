;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname on_tick) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/vector)
(require racket/system)
(require racket/base)
(require "public.rkt")
(require "render.rkt")

(provide (all-defined-out))

;check-I?:

;input/output:
;cor layout -> Boolean

;purpose statement:
;check if the symbol of the given cor in the given layout is 'I
;if it is,returns #t
;otherwise, returns #f
(define (check-I? layout cor)
  (eq? (get-symbol layout cor) 'I))

;check-D?:

;input/output:
;cor layout -> Boolean

;purpose statement:
;check if the symbol of the given cor in the given layout is 'D
;if it is,returns #t
;otherwise, returns #f
(define (check-D? layout cor)
  (eq? (get-symbol layout cor) 'D))


;single-boom-range:

;input/output:
;bombstate layout -> list<Cor>

;purpose statement:
;calculate the boom-range of the given single bombstate
;call extend-direction function to calculate the boom-range of each direction
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
       ;;use apply append to convert the (list (list<Cor)(list<Cor)(list<Cor)(list<Cor))
       ;;to a single list<Cor>



;extend-direction:

;date types:
;potential-list is a list<Cor>
;represents the max-boom-range in each direction

;input/output:
;potential-list layout -> list<Cor>

;purpose statement:
;extend the explosion range in each direction
;the extending rule is:
;when meet first 'D , add the Cor of 'D to the output list<Cor> but
;in this direction
;when meet first 'I , directly stop extending in this direction.
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

;input/output:
;list<Bombstate> layout -> list<Cor>

;purpose statement:
;calculate the boom-range of a list<Bombstate>
(define (boom-range bomb-list layout)
  (apply append
         (map
          (lambda (bombstate)
                (single-boom-range bombstate layout))
              bomb-list)))

;boom:

;input/output:
;gamestate -> gamestate

;purpose statement:
;filter the count-down=0 List<Bomb>, calculate the exploding range,
;by chain effect calculate the new exploding range(bomb1 -> bomb2 -> bomb3),
;put the total exploding cors into accumulator,
;update the layout according to exploding cors,
;update the players state(dead?)

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
             [exploding-list ;the bombs those count-down = 0 AND not in the processed-bombs list
              (filter
               (lambda (bombstate)
                 (and (= (bombstate-countdown bombstate) 0) ;
                      (not (in-boom-cor? (bombstate-cor bombstate) processed-bombs))))
               current-bomb-list)] 
             [boom-cor (boom-range exploding-list current-layout)] ;the list of cors of bombs in exploding list on the current layout
             [updated-bomb-list (chain-explosion boom-cor current-bomb-list)] ;updated List<Bomb> under chain explosion effect
             [new-processed-bombs (append processed-bombs (map bombstate-cor exploding-list))] ;extract the cors from the bombs in exploding list
             ; thus the processed-bombs should be a list of cors
             [new-layout
              (cond
                [(and
                  (in-boom-cor? player1-cor boom-cor)
                  (in-boom-cor? player2-cor boom-cor))
                  
                  (convert ;;kill player2 ;convert1
                   (convert ;;kill player1 ;convert2
                    (convert current-layout boom-cor 'E2) ;;change boom-range to 'E2 ;convert3 ;layout for convert2
                   player1-cor ;cor for convert2
                   (string->symbol (string-append "E" "1" player1-direction)))
                   ;convert2 the cor which has player1 into symbol 'E1direction ;symbol for convert2; layout for convert3
                  
                  player2-cor ;cor for convert3
                  (string->symbol (string-append "E" "2" player2-direction)))]
                ;convert3 the cor which has player2 into symbol 'E2direction 

                [(in-boom-cor? player1-cor boom-cor)
                 
                 (convert ;;kill player1
                  (convert current-layout boom-cor 'E2) ;;change boom-range to 'E2
                  player1-cor (string->symbol (string-append "E" "1" player1-direction)))]
                
                [(in-boom-cor? player2-cor boom-cor)
                 
                 (convert ;;kill player2
                  (convert current-layout boom-cor 'E2) ;;change boom-range to 'E2
                 player2-cor (string->symbol (string-append "E" "2" player2-direction)))]

                [else (convert current-layout boom-cor 'E2)])] ;;change boom-range to 'E2
             
             [new-accumulated-boom-cor (append accumulated-boom-cor boom-cor)] ;start: '()+list of cors          
             )
        (if (empty? exploding-list) ;no bombs count down is 0, or they are all in processed-bomb-list      
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

;input/output:
;list<Cor> list<Bombstate> -> list<Bombstate>

;purpose statement:
;handle the chain-explosion,the rule is:
;if a bomb's Cor matches any Cor in the initial-boom-cor (a list<Cor>)
;and its countdown is not 0
;this bomb's countdown will be set to 0
;otherwise,the bomb will be unchanged
;in final, returns the updated list<Bombstate> after applying the rule to all
;the element in the given bomb-list
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

;input/output:
;Cor Cor -> Boolean

;purpose statement:
;returns #t if the given two cors has both the same column and row
;otherwise returns #f
(define (cor=? cor1 cor2)
  (and (= (cor-column cor1) (cor-column cor2))
       (= (cor-row cor1) (cor-row cor2))))


;in-boom-cor?:

;input/output:
;Cor list<Cor> -> Boolean

;purpose statement:
;returns #t if the given cor is in the given list<Cor>
;otherwise returns #f
(define (in-boom-cor? cor cor-list)
  (cond
    [(empty? cor-list) #f]
    [else
     (if
      (cor=? cor (first cor-list))
      #t
      (in-boom-cor? cor (rest cor-list)))]))
  

      
;remove-bomb:

;input/output:
;list<bombstate> -> list<bombstate>

;purpose statement:
;remove the bombstate which countdown is 0 in the giveb bomb-list

(define (remove-bomb bomb-list)
  (filter (lambda (bomb)
            (> (bombstate-countdown bomb) 0)) bomb-list))


;updated-roundtimer:

;input/output:
;Number -> Number

;purpose statement:
;decreases the roundtimer by 1 in each tick,
;but the roundtimer doesn't go below 0

(define (updated-roundtimer roundtimer)
  (max 0 (sub1 roundtimer))) 


;check-roundtimer?:

;input/output:
;Number -> Boolean

;purpose statement:
;returns #t if the roundtimer is divisible by 30 (except 120 or 0)
;otherwise returns #f

(define (check-roundtimer? roundtimer)
  (and
   (not (= roundtimer 120))
   (not (= roundtimer 0))    
   (= (modulo roundtimer 30) 0)))


;add-maximum:

;input/output:
;Number -> Number

;purpose statement:
;increases the given maximum value by 1
;but maximum cannot exceed 6

(define (add-maximum maximum)
  (min 6 (add1 maximum)))


;updated-bomb:
;input/output:
;list<bombstate> -> list<bombstate>

;purpose statement:
;updates the countdown of each bombstate in the bomb-list (sub1 in each tick)
;but the countdown cannot go below 0

(define (updated-bomb bomb)
  (map (lambda (bombstate)
         (make-bombstate
          (bombstate-cor bombstate)
          (max 0 (sub1 (bombstate-countdown bombstate)))
          (bombstate-owner bombstate)))
       bomb))


;check-zero?:
;input/output:
;list<bombstate> -> Boolean

;purpose statement:
;returns #t if there is any bombstate in the bomb-list has countdown equal to 0
;otherwise returns #f

(define (check-zero? bomb-list)
  (cond
    [(empty? bomb-list) #f]
    [else
     (if
     (= (bombstate-countdown (first bomb-list)) 0)
     #t
     (check-zero? (rest bomb-list)))]))


;update-T-symbols:

;input/output:
;symbol -> symbol

;purpose statement:
;change the symbol in sequence 'E2->'E1->'E0->'W in each tick
(define (update-T-symbols symbol)
  (cond
    [(equal? symbol 'E2) 'E1]
    [(equal? symbol 'E1) 'E0]
    [(equal? symbol 'E0) 'W]
    [else symbol]))

;updated-layout:

;input/output:
;layout -> layout

;purpose statement:
;updates the layout by applying the update-T-symbols function to every symbol
;in the layout
(define (updated-layout layout)
  (vector-map
   (lambda (row)
           (vector-map update-T-symbols row)) 
   layout)) 

;timehandler:

;input/output
;gamestate -> gamestate

;purpose statement:

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