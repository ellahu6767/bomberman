;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname on_key) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/vector)
(require racket/system)
(require racket/base)
(require racket/string)
(require "public.rkt")

(provide (all-defined-out))

;Layout Cor -> String
;get the 1st letter of the symbol at this cor
(define (1st-letter layout cor)
  (string-ith (symbol->string (get-symbol layout cor)) 0))

;Layout Cor -> Boolean
;is this cor a walkable cor?
(define (move-predicate? layout cor)
  (and (in-bound? cor layout)
       (not (string-contains? (symbol->string (get-symbol layout cor)) "W1"))
       (not (string-contains? (symbol->string (get-symbol layout cor)) "W2"));P1 and P2 cannot stay in the same cell
       (or (equal? (1st-letter layout cor) "W") ; aisle is walkable
           (equal? (1st-letter layout cor) "E")))) ; explosion area is also walkable

;Gamestate Cor Owner -> Boolean
;can this player put this bomb?
(define (put-predicate? gamestate current-cor owner)
  (let ([nbomb (count-num (filter ;how many bombs belong to this owner
                                  (lambda(bombstate)
                                    (symbol=? (bombstate-owner bombstate) owner)) ;the bombs those belong to player1
                                  (gamestate-bomb gamestate)))])
  (and (< nbomb (gamestate-maximum gamestate))
       (not (equal? "B" (1st-letter (gamestate-layout gamestate) current-cor)))))) ;cannot put multiple bombs on one cell

;Cor direction -> Cor
;get the new-cor after 1-step movement, assuming movement is possible
(define (renew-cor old-cor direction)
  (cond
    [(equal? direction "D") (make-cor (cor-column old-cor) (+ 1 (cor-row old-cor)))]
    [(equal? direction "U") (make-cor (cor-column old-cor) (- (cor-row old-cor) 1))]
    [(equal? direction "L") (make-cor (- (cor-column old-cor) 1) (cor-row old-cor))]
    [(equal? direction "R") (make-cor (+ 1 (cor-column old-cor)) (cor-row old-cor))]
    [else old-cor]))

;Who is one of the following String:
;- "1"
;- "2"
;Interpretation: shortcuts for the player, "1" stays for Player1, "2" stays for Player2

;Gamestate Layout Cor Direction Who -> Gamestate
;put the bomb assuming put-predicate? is #true
(define (put-bomb gamestate old-layout current-cor current-direction who) ;suppose put-predicate? is #true
  (let* ([new-symbol (string->symbol (string-append "B" who current-direction))] ;player with bomb
         [new-layout (convert old-layout current-cor new-symbol)]
         [new-bomb-owner (string->symbol (string-append "P" who))]
         [new-bomb (make-bombstate current-cor 3 new-bomb-owner)] ;set timer to 3
         [new-bomb-list (cons new-bomb (gamestate-bomb gamestate))]) ;new bomb-list
    (make-gamestate
     new-layout
     new-bomb-list
     (gamestate-player1 gamestate)
     (gamestate-player2 gamestate)
     (gamestate-roundtimer gamestate)
     (gamestate-maximum gamestate)
     (gamestate-quit? gamestate))))

;Gamestate Cor Symbol Layout Who Direction -> Gamestate
;the gamestate after one movement is done
(define (move gamestate old-cor old-symbol old-layout who direction)
  (let* ([new-cor (renew-cor old-cor direction)]
         [can-move? (move-predicate? old-layout new-cor)]
         [new-symbol (if can-move?
                          (string->symbol (string-append (1st-letter old-layout new-cor) who direction))
                          ;can-move? change cor and direction
                          (string->symbol (string-append (1st-letter old-layout old-cor) who direction)))]
                          ;no can-move, change only dir
         [restore-symbol (string->symbol (1st-letter old-layout old-cor))]
         [new-layout (if can-move?                 
                         (convert (convert old-layout old-cor restore-symbol) new-cor new-symbol)
                         ;can-move?, restore old cell and change the new cell
                         (convert old-layout old-cor new-symbol)
                         ;else, change only direction
                         )])
    (make-gamestate
     new-layout
     (gamestate-bomb gamestate)
     (if (equal? who "1")
         (make-player1 (if can-move? new-cor old-cor) direction) ;P1 move?
         (gamestate-player1 gamestate))
     (if (equal? who "2")
         (make-player2 (if can-move? new-cor old-cor) direction) ;P2 move?
         (gamestate-player2 gamestate))
     (gamestate-roundtimer gamestate)
     (gamestate-maximum gamestate)
     (gamestate-quit? gamestate))))

;Gamestate Key -> Gamestate
;pressing q key will quit the game;
;pressing space key from homepage will enter the intial random generated layout;
;pressing "down" "up" "left" "right" player1 will move down, up, left, right;
;pressing "w" "a" "s" "d", player2 will move up, left, down, right.
;pressing space key in gamepage, player1 puts a bomb.
;pressing "g", player2 puts a bomb.
(define (keyhandler gamestate ke)
  (cond
    [(string=? ke "q") (make-gamestate
                        (gamestate-layout gamestate)
                        (gamestate-bomb gamestate)
                        (gamestate-player1 gamestate)
                        (gamestate-player2 gamestate)
                        (gamestate-roundtimer gamestate)
                        (gamestate-maximum gamestate)
                        #t)]
    [(equal? (gamestate-layout gamestate) homepage) (if (string=? ke " ")
                                                        initial-state
                                                        gamestate)]
    [else (let* ([old-layout (gamestate-layout gamestate)]
                 [old-cor1 (player1-cor (gamestate-player1 gamestate))]
                 [old-cor2 (player2-cor (gamestate-player2 gamestate))]
                 [old-symbol1 (get-symbol old-layout old-cor1)]
                 [old-symbol2 (get-symbol old-layout old-cor2)])
            (cond
              [(string=? ke "down") (move gamestate old-cor1 old-symbol1 old-layout "1" "D")]
              [(string=? ke "up") (move gamestate old-cor1 old-symbol1 old-layout "1" "U")]
              [(string=? ke "left") (move gamestate old-cor1 old-symbol1 old-layout "1" "L")]
              [(string=? ke "right") (move gamestate old-cor1 old-symbol1 old-layout "1" "R")]
              [(string=? ke "s") (move gamestate old-cor2 old-symbol2 old-layout "2" "D")]
              [(string=? ke "w") (move gamestate old-cor2 old-symbol2 old-layout "2" "U")]
              [(string=? ke "a") (move gamestate old-cor2 old-symbol2 old-layout "2" "L")]
              [(string=? ke "d") (move gamestate old-cor2 old-symbol2 old-layout "2" "R")]
              [(string=? ke " ") (let* ([current-direction (player1-direction (gamestate-player1 gamestate))]
                                        [can-put? (put-predicate? gamestate old-cor1 'P1)])
                                   (if can-put?
                                       (put-bomb gamestate old-layout old-cor1 current-direction "1")
                                       ;if can-put, then return the new gamestate, otherwise the old one
                                       gamestate))]
              [(string=? ke "g") (let* ([current-direction (player2-direction (gamestate-player2 gamestate))]
                                        [can-put? (put-predicate? gamestate old-cor2 'P2)])
                                   (if can-put?
                                       (put-bomb gamestate old-layout old-cor2 current-direction "2")
                                       gamestate))]
              [else gamestate]))]))








