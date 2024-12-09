;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname big_bang) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/vector)
(require racket/system)
(require racket/base)
(require "render.rkt")
(require "on_tick.rkt")
(require "on_key.rkt")
(require "stop_when.rkt")
(require "public.rkt")



;main function

(define (main gamestate)
  (big-bang gamestate
    [to-draw render]
    [on-key keyhandler]
    [on-tick timehandler 0.5]
    [stop-when end?
               final]
))

;application
(system "open ~/Desktop/backup/background.mp3")
(main homepage-state)
