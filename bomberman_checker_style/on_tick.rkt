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

(provide timehandler)

(define-struct gamestate [layout bomb player1 player2 roundtimer maximum quit?] #:transparent)
(define-struct bombstate [cor countdown owner] #:transparent)
(define-struct cor [column row] #:transparent)
(define-struct player1 [cor direction] #:transparent)
(define-struct player2 [cor direction] #:transparent)

;constant definitions
(define BOOM-MAX-DISTANCE 2)

;cor layout -> Boolean
(define (check-I? layout cor)
  (eq? (get-symbol layout cor) 'I))

;cor layout -> Boolean
(define (check-D? layout cor)
  (eq? (get-symbol layout cor) 'D))

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
      [(> n BOOM-MAX-DISTANCE) acc] ;return the list acc when n=3
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
           (lambda (direction-fn)
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
             [updated-bomb-list (chain-explosion boom-cor current-bomb-list current-layout)] ;updated List<Bomb> under chain explosion effect
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
;list-of-cor bomb-list layout -> bomb-list
(define (chain-explosion initial-boom-cor bomb-list)
    (map
     (lambda (bombstate)
             (if 
               (and
                (not (= (bombstate-countdown bombstate) 0))
                (in-boom-cor? (bombstate-cor bombstate) initial-boom-cor)) ;a bomb which count-down isnt 0, but is in boom range

                (make-bombstate (bombstate-cor bombstate)
                                0
                                (bombstate-owner bombstate)) ;set this bomb count-down to 0
             
                bombstate))
     
              bomb-list)
    ) ;return the updated bomb list under chain explosion effect

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
  (filter (lambda (bomb)
            (> (bombstate-countdown bomb) 0)) bomb-list))


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
   (lambda (row)
           (vector-map update-T-symbols row)) ;every symbol on this row is gonna to be updated
   layout)) ;every row in this layout will update their symbols
         
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