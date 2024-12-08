;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname public) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/vector)
(require racket/system)
(require racket/base)

(provide initial-player1)
(provide initial-player2)
(provide initial-roundtimer)
(provide initial-maximum)
(provide MAX-ROWS)
(provide MAX-COLS)
(provide get-symbol)
(provide in-bound?)
(provide convert)

;structure
(define-struct gamestate [layout bomb player1 player2 roundtimer maximum quit?] #:transparent)
(define-struct bombstate [cor countdown owner] #:transparent)
(define-struct cor [column row] #:transparent)
(define-struct player1 [cor direction] #:transparent)
(define-struct player2 [cor direction] #:transparent)

;constants
(define MAX-ROWS 11) 
(define MAX-COLS 15)
(define initial-player1 (make-player1
                        (make-cor 0 0)
                        "D"))
(define initial-player2 (make-player2
                        (make-cor (- MAX-COLS 1) (- MAX-ROWS 1))
                        "U"))
(define initial-roundtimer 120)
(define initial-maximum 3)

;cor layout -> symbol
(define (get-symbol layout cor)
  (if (in-bound? cor)
  (vector-ref
   (vector-ref layout (cor-row cor)) ;the row number
   (cor-column cor)) ;the column number
  'ILEGAL)) ;else 'ILL

;cor layout -> Boolean
(define (in-bound? cor layout)
  (and (<= 0 (cor-row cor) (- (vector-length layout) 1))     
       (<= 0 (cor-column cor) (- (vector-length (vector-ref layout 0)) 1))))


;convert:
;data types
;layout is a vector
;cor is a cor or list of cor
(define (convert layout cor symbol)
  (cond
    [(empty? cor) layout]
    [(list? cor)
    (convert
     (convert layout (first cor) symbol) ;convert every element of the coordinate
     (rest cor)
    symbol)]
    [else
     (begin
       (vector-set!
        (vector-ref layout
                    (cor-row cor)) ;the row of layout
        (cor-column cor) ;the column of this row: a specific symbol
        symbol) ;convert the cor symbol on the layout into this symbol
       layout)])) ;return layout