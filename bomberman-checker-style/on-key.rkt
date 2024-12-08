;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname on-key) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/base)
(require "bomberman.rkt")
(require rackunit)
(require rackunit/text-ui)
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
        (symbol=? new-symbol 'W) ;walkable
        (= (string-length (symbol->string new-symbol)) 2))))) ;'E0, 'E1, 'E2
;;Test
(define (test-move-predicate?)
  (test-suite
   "Testing move-predicate?"
   
   (test-case "Valid move to walkable position"
     (check-true (move-predicate? '() '(1 1))))
   
   (test-case "Valid move to destructible position"
     (check-true (move-predicate? '() '(2 2))))
   
   (test-case "Invalid move to non-walkable position"
     (check-false (move-predicate? '() '(3 3))))
   
   (test-case "Valid move to destructible object E0"
     (check-true (move-predicate? '() '(4 4))))))

;; put-predicate?: layout cor owner maximum
(define (put-predicate? layout current-cor owner maximum)
  (let (
        [current-symbol (get-symbol layout current-cor)]
        )
    (and (< owner maximum) ;is the bomb number < maximum
         (= (string-length (symbol->string current-symbol)) 3)))) ;is 'W[1|2][U|L|R|D]
;;Test
(define (test-put-predicate?)
  (test-suite
   "Testing put-predicate?"
   
   (test-case "Valid bomb placement (owner1 < maximum)"
     (check-true (put-predicate? '() '(4 4) 2 3)))
   
   (test-case "Invalid bomb placement (owner1 >= maximum)"
     (check-false (put-predicate? '() '(4 4) 3 3)))
   
   (test-case "Invalid bomb placement at a non-bomb position"
     (check-false (put-predicate? '() '(1 1) 1 3)))))

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
         
         [current-cor2 (player2-cor (gamestate-player2 gamestate))] ;p2
         [current-symbol2 (get-symbol layout current-cor2)] ;p2
         [current-direction2 (player2-direction (gamestate-player2 gamestate))] ;p2
         [current-cor2-column (cor-column current-cor2)] ;p2
         [current-cor2-row (cor-row current-cor2)] ;p2
         )
    
    (cond
      
      ;;down
      [(string=? ke "down") ;p1 down
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
                               ;take the first symbol of the current-cor, where the player 1 is
                               current-symbol)]
                          )
                                                      
                     (convert
                      (convert layout new-cor final-symbol)
                      current-cor restored-symbol)) ;restored-symbol on the layout

                   (convert layout current-cor ;else
                            (string->symbol
                             (string-append
                              (string-ith (symbol->string current-symbol) 0) ;current-cor, unchanged
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

      [(string=? ke "s") ;p2 down
       (let* (
              [new-cor (make-cor current-cor2-column (+ current-cor2-row 1))]
              [new-direction "D"]            
              [new-symbol (string->symbol (string-ith (symbol->string (get-symbol layout new-cor)) 0))]
              [final-symbol (string->symbol
                             (string-append (symbol->string new-symbol) "2" new-direction))]
              [can-move (move-predicate? layout new-cor)]
              [updated-layout
               (if can-move
                   (let* (
                          [restored-symbol 
                           (if (= (string-length (symbol->string current-symbol2)) 3)
                               (string->symbol (string-ith (symbol->string current-symbol2) 0))
                               current-symbol2)]
                          )
                                                      
                     (convert 
                      (convert layout new-cor final-symbol)
                      current-cor2 restored-symbol))

                   (convert layout current-cor2
                            (string->symbol
                             (string-append
                              (string-ith (symbol->string current-symbol2) 0)
                              "2"                                   
                              new-direction))))]                    

              )               
         (make-gamestate 
          updated-layout
          (gamestate-bomb gamestate)
          (gamestate-player1 gamestate)
          (make-player2
           (if can-move
               new-cor
               current-cor2)
           new-direction) 
          (gamestate-roundtimer gamestate)
          (gamestate-maximum gamestate)
          (gamestate-quit? gamestate)))]   

     ;;up
      [(string=? ke "up") ;p1 up
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
                              (string-ith (symbol->string current-symbol) 0)
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

      [(string=? ke "w") ;p2 up
       (let* (
              [new-cor (make-cor current-cor2-column (- current-cor2-row 1))]
              [new-direction "U"]
              [new-symbol (string->symbol (string-ith (symbol->string (get-symbol layout new-cor)) 0))]
              [final-symbol (string->symbol
                             (string-append (symbol->string new-symbol) "2" new-direction))]
              [can-move (move-predicate? layout new-cor)]
              [updated-layout
               (if can-move
                   (let* ([restored-symbol
                           (if (= (string-length (symbol->string current-symbol2)) 3)
                               (string->symbol (string-ith (symbol->string current-symbol2) 0))
                               current-symbol2)]
                          )
                     (convert
                      (convert layout new-cor final-symbol)
                      current-cor2 restored-symbol))
                   (convert layout current-cor2
                            (string->symbol
                             (string-append
                              (string-ith (symbol->string current-symbol2) 0)
                              "2"
                              new-direction))))]                       
          )               
     (make-gamestate
          updated-layout
          (gamestate-bomb gamestate)
          (gamestate-player1 gamestate)
          (make-player2
           (if can-move
               new-cor
               current-cor2)
           new-direction) 
          (gamestate-roundtimer gamestate)
          (gamestate-maximum gamestate)
          (gamestate-quit? gamestate)))]

      ;;left
      [(string=? ke "left") ;p1 left
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
                              (string-ith (symbol->string current-symbol) 0)
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

      [(string=? ke "a") ;p2 left
       (let* (
              [new-cor (make-cor (- current-cor2-column 1) current-cor2-row)]
              [new-direction "L"]
              [new-symbol (string->symbol (string-ith (symbol->string (get-symbol layout new-cor)) 0))]
              [final-symbol (string->symbol
                         (string-append (symbol->string new-symbol) "2" new-direction))]
              [can-move (move-predicate? layout new-cor)]
              [updated-layout
               (if can-move
                   (let* ([restored-symbol
                           (if (= (string-length (symbol->string current-symbol2)) 3)
                               (string->symbol (string-ith (symbol->string current-symbol2) 0))
                               current-symbol2)]
                          )
                     
                     (convert
                      (convert layout new-cor final-symbol)
                      current-cor2 restored-symbol))
                   (convert layout current-cor2
                            (string->symbol
                             (string-append
                              (string-ith (symbol->string current-symbol2) 0)
                              "2"
                              new-direction))))]                       
          )               
     (make-gamestate
          updated-layout
          (gamestate-bomb gamestate)
          (gamestate-player1 gamestate)
          (make-player2
           (if can-move
               new-cor
               current-cor2)
           new-direction)           
          (gamestate-roundtimer gamestate)
          (gamestate-maximum gamestate)
          (gamestate-quit? gamestate)))] 

  ;;right
      [(string=? ke "right") ;p1 right
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
                              (string-ith (symbol->string current-symbol) 0)
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

      [(string=? ke "d") ;p2 right
       (let* (
              [new-cor (make-cor (+ current-cor2-column 1) current-cor2-row)]
              [new-direction "R"]
              [new-symbol (string->symbol (string-ith (symbol->string (get-symbol layout new-cor)) 0))]
              [final-symbol (string->symbol
                         (string-append (symbol->string new-symbol) "2" new-direction))]
              [can-move (move-predicate? layout new-cor)]
              [updated-layout
               (if can-move
                   (let* ([restored-symbol
                           (if (= (string-length (symbol->string current-symbol2)) 3)
                               (string->symbol (string-ith (symbol->string current-symbol2) 0))
                               current-symbol2)]

                          )
                     
                     (convert
                      (convert layout new-cor final-symbol)
                      current-cor2 restored-symbol))
                   (convert layout current-cor2
                            (string->symbol
                             (string-append
                              (string-ith (symbol->string current-symbol2) 0)
                              "2"
                              new-direction))))]                       
          )               
     (make-gamestate
          updated-layout
          (gamestate-bomb gamestate)
          (gamestate-player1 gamestate)
          (make-player2
           (if can-move
               new-cor
               current-cor2)
           new-direction) 
          (gamestate-roundtimer gamestate)
          (gamestate-maximum gamestate)
          (gamestate-quit? gamestate)))]
      
      ;; put-bomb
      [(string=? ke " ") ;p1 bomb
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

      [(string=? ke "g") ;p2 bomb
       (let* (
              [final-symbol (string->symbol
                             (string-append "B" "2" current-direction2))]
              [bomb-list (gamestate-bomb gamestate)]
              [owner2 (count-num (filter
                                  (lambda(bombstate)
                                    (symbol=? (bombstate-owner bombstate) 'P2)) 
                                  bomb-list))]
              [can-put? (put-predicate? layout current-cor2 owner2 maximum)] 
              [updated-layout
               (if can-put?                   
                   (convert layout current-cor2 final-symbol)
                   layout)] 
              [updated-bomb-list
               (if can-put?
                    (cons (make-bombstate current-cor2 3 'P2) (gamestate-bomb gamestate))
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
   
      [else gamestate]))]))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;keyhandler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;