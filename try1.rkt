;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname try1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/string)

;layout cor -> boolean
;is this cor a walkable cor?
(define (move-predicate? layout cor)
  (and (in-bound? cor)
       (string-contains? (symbol->string (get-symbol layout cor)) (or "W" "E"))))
  
;layout cor bomb-list maximum -> boolean
(define (put-predicate? gamestate cor player)
  (let [nbomb (count-num (filter (lambda (bombstate)
            (string-contains? (symbol->string (bombstate-owner bomstate)) player))
          (gamestate-bomb gamestate)))])
  (and (< nbomb (gamestate-maximum maximum))
       (= (string-length (symbol->string (get-symbol (gamestate-layout gamestate) cor))) 3)))
        


;key -> string
;new direction after pressing the key
(define (new-direction ke)
  (cond
    [(equal? ke (or "s" "down")) "D"]
    [(equal? ke (or "w" "up")) "U"]
    [(equal? ke (or "a" "left")) "L"]
    [(equal? ke (or "d" "right")) "R"]))

;key -> string
;who moves
(define (who-moves? ke)
  (cond
    [(equal? ke (or "s" "w" "a" "d" "g")) "2"]
    [(equal? ke (or "down" "up" "left" "right" " ")) "1"]))

;key cor -> cor
;new cor after moving, no matter movable or not
(define (new-cor ke current-cor layout)
  (cond
    [(equal? (new-direction ke) "D") (make-cor (cor-column current-cor) (add1 (cor-row current-cor))) ]
    [(equal? (new-direction ke) "U") (make-cor (cor-column current-cor) (sub1 (cor-row current-cor))) ]
    [(equal? (new-direction ke) "L") (make-cor (sub1 (cor-column current-cor)) (cor-row current-cor)) ]
    [(equal? (new-direction ke) "R") (make-cor (add1 (cor-column current-cor)) (cor-row current-cor)) ]))

;key -> symbol
;new symbol on layout
(define (new-symbol gamestate cor ke)
  (string-append "W" (who-moves? ke) (new-direction ke)))

;new-layout
(define (new-layout gamestate cor)
  (cond
    [(move-predicate? (gamestate-layout gamestate) cor) (local ((define ]




















 

