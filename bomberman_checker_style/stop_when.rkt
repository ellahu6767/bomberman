;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname stop_when) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
      (check-all-died? player1-symbol player2-symbol)
      (check-player1-died? player1-symbol) 
      (check-player2-died? player2-symbol)
      time-finish?))]))))



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