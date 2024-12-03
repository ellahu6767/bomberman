;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname big-bang) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
