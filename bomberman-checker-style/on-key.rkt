;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname on-key) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

(define (put-predicate? layout current-cor owner1 maximum)
  (let (
        [current-symbol (get-symbol layout current-cor)]
        )
    (and (< owner1 maximum) ;is the bomb number < maximum
         (= (string-length (symbol->string current-symbol)) 3)))) ;is 'W[1|2][U|L|R|D]


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
       #t) ;quit is #true
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
              [new-symbol (string->symbol (string-ith (symbol->string (get-symbol layout new-cor)) 0))]
              [final-symbol (string->symbol
                             (string-append (symbol->string new-symbol) "1" new-direction))]
              [can-move (move-predicate? layout new-cor)]
              [updated-layout
               (if can-move
                   (let* (
                          [restored-symbol
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
                    (cons (make-bombstate current-cor 3 'P1) (gamestate-bomb gamestate))
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