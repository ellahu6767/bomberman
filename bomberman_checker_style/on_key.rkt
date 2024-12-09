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
(require "render.rkt")



(provide (all-defined-out))

;layout cor -> boolean
;is this cor a walkable cor?
(define (move-predicate? layout cor)
  (and (in-bound? cor layout)
       (or (string-contains? (symbol->string (get-symbol layout cor)) "W")
           (string-contains? (symbol->string (get-symbol layout cor)) "E"))))
  
;gamestate cor who -> boolean
;who should be "1" or "2"
(define (put-predicate? gamestate current-cor who-symbol)
  (let ([nbomb (count-num (filter ;how many bombs belong to who-symbol
                                  (lambda(bombstate)
                                    (symbol=? (bombstate-owner bombstate) who-symbol)) ;the bombs those belong to player1
                                  (gamestate-bomb gamestate)))])
  (and (< nbomb (gamestate-maximum gamestate))
       (not (equal? "B" (1st-letter (gamestate-layout gamestate) current-cor)))))) ;cannot put multiple bombs on one cell

(define (1st-letter layout cor)
  (string-ith (symbol->string (get-symbol layout cor)) 0))

(define (put-bomb gamestate old-layout current-cor current-direction who) ;suppose put-predicate? is #true
  (let* ([new-symbol (string->symbol (string-append "B" who current-direction))]
         [new-layout (convert old-layout current-cor new-symbol)]
         [new-bomb-owner (string->symbol (string-append "P" who))]
         [new-bomb (make-bombstate current-cor 3 new-bomb-owner)]
         [new-bomb-list (cons new-bomb (gamestate-bomb gamestate))])
    (make-gamestate
     new-layout
     new-bomb-list
     (gamestate-player1 gamestate)
     (gamestate-player2 gamestate)
     (gamestate-roundtimer gamestate)
     (gamestate-maximum gamestate)
     (gamestate-quit? gamestate))))
                 
;gamestate -> gamestate
;move-down player1
;who: "1" or "2"
(define (move-down gamestate old-cor old-symbol old-layout who)
  (let* ([new-cor (make-cor (cor-column old-cor) (+ 1 (cor-row old-cor)))]
         [can-move? (move-predicate? old-layout new-cor)]
         [new-symbol (if can-move?
                          (string->symbol (string-append (1st-letter old-layout new-cor) who "D"))
                          (string->symbol (string-append (1st-letter old-layout old-cor) who "D")))]
         [restore-symbol (string->symbol (1st-letter old-layout old-cor))]
         [new-layout (if can-move?                 
                         (convert (convert old-layout old-cor restore-symbol) new-cor new-symbol)
                         (convert old-layout old-cor new-symbol)
                         )])
    (make-gamestate
     new-layout
     (gamestate-bomb gamestate)
     (if (equal? who "1")
         (make-player1 (if can-move? new-cor old-cor) "D")
         (gamestate-player1 gamestate))
     (if (equal? who "2")
         (make-player2 (if can-move? new-cor old-cor) "D")
         (gamestate-player2 gamestate))
     (gamestate-roundtimer gamestate)
     (gamestate-maximum gamestate)
     (gamestate-quit? gamestate))))

(define (move-up gamestate old-cor old-symbol old-layout who)
  (let* ([new-cor (make-cor (cor-column old-cor) (- (cor-row old-cor) 1))]
         [can-move? (move-predicate? old-layout new-cor)]
         [new-symbol (if can-move?
                          (string->symbol (string-append (1st-letter old-layout new-cor) who "U"))
                          (string->symbol (string-append (1st-letter old-layout old-cor) who "U")))]
         [restore-symbol (string->symbol (1st-letter old-layout old-cor))]
         [new-layout (if can-move?
                         (convert (convert old-layout old-cor restore-symbol) new-cor new-symbol)
                         (convert old-layout old-cor new-symbol))])
    (make-gamestate
     new-layout
     (gamestate-bomb gamestate)
     (if (equal? who "1")
         (make-player1 (if can-move? new-cor old-cor) "U")
         (gamestate-player1 gamestate))
     (if (equal? who "2")
         (make-player2 (if can-move? new-cor old-cor) "U")
         (gamestate-player2 gamestate))
     (gamestate-roundtimer gamestate)
     (gamestate-maximum gamestate)
     (gamestate-quit? gamestate))))

(define (move-left gamestate old-cor old-symbol old-layout who)
  (let* ([new-cor (make-cor (- (cor-column old-cor) 1) (cor-row old-cor))]
         [can-move? (move-predicate? old-layout new-cor)]
         [new-symbol (if can-move?
                          (string->symbol (string-append (1st-letter old-layout new-cor) who "L"))
                          (string->symbol (string-append (1st-letter old-layout old-cor) who "L")))]
         [restore-symbol (string->symbol (1st-letter old-layout old-cor))]
         [new-layout (if can-move?
                         (convert (convert old-layout old-cor restore-symbol) new-cor new-symbol)
                         (convert old-layout old-cor new-symbol))])
    (make-gamestate
     new-layout
     (gamestate-bomb gamestate)
     (if (equal? who "1")
         (make-player1 (if can-move? new-cor old-cor) "L")
         (gamestate-player1 gamestate))
     (if (equal? who "2")
         (make-player2 (if can-move? new-cor old-cor) "L")
         (gamestate-player2 gamestate))
     (gamestate-roundtimer gamestate)
     (gamestate-maximum gamestate)
     (gamestate-quit? gamestate))))

(define (move-right gamestate old-cor old-symbol old-layout who)
  (let* ([new-cor (make-cor (+ 1 (cor-column old-cor)) (cor-row old-cor))]
         [can-move? (move-predicate? old-layout new-cor)]
         [new-symbol (if can-move?
                          (string->symbol (string-append (1st-letter old-layout new-cor) who "R"))
                          (string->symbol (string-append (1st-letter old-layout old-cor) who "R")))]
         [restore-symbol (string->symbol (1st-letter old-layout old-cor))]
         [new-layout (if can-move?
                         (convert (convert old-layout old-cor restore-symbol) new-cor new-symbol)
                         (convert old-layout old-cor new-symbol))])
    (make-gamestate
     new-layout
     (gamestate-bomb gamestate)
     (if (equal? who "1")
         (make-player1 (if can-move? new-cor old-cor) "R")
         (gamestate-player1 gamestate))
     (if (equal? who "2")
         (make-player2 (if can-move? new-cor old-cor) "R")
         (gamestate-player2 gamestate))
     (gamestate-roundtimer gamestate)
     (gamestate-maximum gamestate)
     (gamestate-quit? gamestate))))

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
    [else (let ([old-layout (gamestate-layout gamestate)])
            (cond
              [(string=? ke "down") (let* ([old-cor (player1-cor (gamestate-player1 gamestate))]
                                           [old-symbol (get-symbol old-layout old-cor)])
                                      (move-down gamestate old-cor old-symbol old-layout "1"))]
              [(string=? ke "up") (let* ([old-cor (player1-cor (gamestate-player1 gamestate))]
                                         [old-symbol (get-symbol old-layout old-cor)])
                                    (move-up gamestate old-cor old-symbol old-layout "1"))]
              [(string=? ke "left") (let* ([old-cor (player1-cor (gamestate-player1 gamestate))]
                                           [old-symbol (get-symbol old-layout old-cor)])
                                      (move-left gamestate old-cor old-symbol old-layout "1"))]
              [(string=? ke "right") (let* ([old-cor (player1-cor (gamestate-player1 gamestate))]
                                            [old-symbol (get-symbol old-layout old-cor)])
                                       (move-right gamestate old-cor old-symbol old-layout "1"))]
              [(string=? ke "s") (let* ([old-cor (player2-cor (gamestate-player2 gamestate))]
                                        [old-symbol (get-symbol old-layout old-cor)])
                                   (move-down gamestate old-cor old-symbol old-layout "2"))]
              [(string=? ke "w") (let* ([old-cor (player2-cor (gamestate-player2 gamestate))]
                                        [old-symbol (get-symbol old-layout old-cor)])
                                   (move-up gamestate old-cor old-symbol old-layout "2"))]
              [(string=? ke "a") (let* ([old-cor (player2-cor (gamestate-player2 gamestate))]
                                        [old-symbol (get-symbol old-layout old-cor)])
                                   (move-left gamestate old-cor old-symbol old-layout "2"))]
              [(string=? ke "d") (let* ([old-cor (player2-cor (gamestate-player2 gamestate))]
                                        [old-symbol (get-symbol old-layout old-cor)])
                                   (move-right gamestate old-cor old-symbol old-layout "2"))]
              [(string=? ke " ") (let* ([current-cor (player1-cor (gamestate-player1 gamestate))]
                                        [current-direction (player1-direction (gamestate-player1 gamestate))]
                                        [put? (put-predicate? gamestate current-cor 'P1)])
                                   (if put?
                                       (put-bomb gamestate old-layout current-cor current-direction "1")
                                       gamestate))]
              [(string=? ke "g") (let* ([current-cor (player2-cor (gamestate-player2 gamestate))]
                                        [current-direction (player2-direction (gamestate-player2 gamestate))]
                                        [put? (put-predicate? gamestate current-cor 'P2)])
                                   (if put?
                                       (put-bomb gamestate old-layout current-cor current-direction "2")
                                       gamestate))]
              [else gamestate]))]))