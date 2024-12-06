;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname public) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;cor layout -> symbol
(define (get-symbol layout cor)
  (if (in-bound? cor)
  (vector-ref
   (vector-ref layout (cor-row cor)) ;the row number
   (cor-column cor)) ;the column number
  'ILL)) ;else 'ILL

;cor layout -> Boolean
(define (in-bound? cor)
  (and (<= 0 (cor-row cor) (- MAX-ROWS 1)) ;if it is a valid vector ref for row, 0~10
       (<= 0 (cor-column cor) (- MAX-COLS 1)))) ;if it is a valid vector ref for column, 0~14


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