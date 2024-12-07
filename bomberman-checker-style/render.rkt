;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname render) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/base)
(require "bomberman.rkt")
(require rackunit)
(require rackunit/text-ui)

(define MAX-ROWS 11) 
(define MAX-COLS 15)

;row col -> Boolean
;rules for generating 'U in random-layout
(define (is-U? row col)
  (and (even? row)          
       (odd? col)          
       (< 0 row (- MAX-ROWS 1))                   
       (< 0 col (- MAX-COLS 1))))
;;Test
(define MAX-ROWS 10)
(define MAX-COLS 10)
(define is-U?-tests
  (test-suite
   "Tests for is-U?"
   (test-case "Row is even, col is odd, both within bounds"
     (check-equal? (is-U? 2 3) #t))
   (test-case "Row is odd"
     (check-equal? (is-U? 1 3) #f))
   (test-case "Col is out of bounds"
     (check-equal? (is-U? 2 9) #f))))

;row col -> Boolean
;rules for generating 'W1D in random-layout
(define (is-player1? row col)
  (= 0 (+ row col)))
;;Test
(define is-player1?-tests
  (test-suite
   "Tests for is-player1?"
   (test-case "Row and col sum to 0"
     (check-equal? (is-player1? 0 0) #t)) ; 0 + 0 = 0
   (test-case "Row and col do not sum to 0 (positive)"
     (check-equal? (is-player1? 3 1) #f)) ; 3 + 1 = 4
   (test-case "Row and col do not sum to 0 (negative)"
     (check-equal? (is-player1? -1 -1) #f)))) ; -1 + (-1) = -2

;row col -> Boolean
;rules for generating 'W2U in random-layout
(define (is-player2? row col)
  (= (+ (- MAX-ROWS 1) (- MAX-COLS 1)) (+ row col)))
;;Test
(define is-player2?-tests
  (test-suite
   "Tests for is-player2?"
   (test-case "Row and col sum to the target value"
     (check-equal? (is-player2? 9 9) #t)) ; 9 + 9 = 18, which equals MAX-ROWS - 1 + MAX-COLS - 1
   (test-case "Row and col do not sum to the target value (too low)"
     (check-equal? (is-player2? 5 5) #f)) ; 5 + 5 = 10, which is less than the target
   (test-case "Row and col do not sum to the target value (too high)"
     (check-equal? (is-player2? 10 10) #f)))) ; 10 + 10 = 20, which is greater than the target

;row col -> Boolean
;rules for generating fixed 'W in random-layout
(define (is-fixed-W? row col)
  (or (and
       (< 0 (+ row col))
       (<= (+ row col) 2)) 
      (>= (+ row col) (+ (- MAX-ROWS 1) (- MAX-COLS 1) -2))))
;;Test
(define is-fixed-W?-tests
  (test-suite
   "Tests for is-fixed-W?"
   (test-case "Sum of row and col is between 0 and 2"
     (check-equal? (is-fixed-W? 1 1) #t)) ; 1 + 1 = 2, within the range 0 < sum <= 2
   (test-case "Sum of row and col is greater than 2 but less than the upper bound"
     (check-equal? (is-fixed-W? 1 3) #f)) ; 1 + 3 = 4, not within the valid range
   (test-case "Sum of row and col equals upper bound minus 2"
     (check-equal? (is-fixed-W? 8 8) #t)) ; 8 + 8 = 16, meets the upper bound condition
   (test-case "Sum of row and col exceeds the upper bound minus 2"
     (check-equal? (is-fixed-W? 9 9) #f)))) ; 9 + 9 = 18, exceeds the condition

;row col -> Boolean
;rules for generating 'D in homepage
(define (is-YU? row col)
  (or
   ;;Y shape
   (and
    (<= 0 col 2)
    (<= 0 row 2)
    (= col row))
   (and
    (<  2 col 6)
    (<= 0 row 2)
    (<= 2 (- col row) 4)
    (= (+ col row) 4))
   (and
    (= col 2)
    (<= 2 row 4))
   ;;U shape
   (and
    (= col 8)
    (<= 0 row 4))
   (and
    (= col 12)
    (<= 0 row 4))
   (and
    (= row 4)
    (<= 8 col 12))))
;;Test
(define is-YU?-tests
  (test-suite
   "Tests for is-YU?"
   
   ;; Y shape tests
   (test-case "Y shape: col == row, 0 <= col <= 2, 0 <= row <= 2"
     (check-equal? (is-YU? 1 1) #t)) ; col == row, within bounds
   
   (test-case "Y shape: 2 < col < 6, 0 <= row <= 2, col - row between 2 and 4"
     (check-equal? (is-YU? 2 2) #t)) ; 2 < col < 6, col + row == 4
   
   (test-case "Y shape: col == 2, 2 <= row <= 4"
     (check-equal? (is-YU? 3 2) #t)) ; col == 2, 2 <= row <= 4
   
   ;; U shape tests
   (test-case "U shape: col == 8, 0 <= row <= 4"
     (check-equal? (is-YU? 3 8) #t)) ; col == 8, within row bounds
   
   (test-case "U shape: col == 12, 0 <= row <= 4"
     (check-equal? (is-YU? 4 12) #t)) ; col == 12, within row bounds
   
   (test-case "U shape: row == 4, 8 <= col <= 12"
     (check-equal? (is-YU? 4 10) #t)) ; row == 4, col between 8 and 12
   
   ;; Negative test cases (should return #f)
   (test-case "Not a Y or U shape"
     (check-equal? (is-YU? 5 5) #f)) ; Neither Y nor U shape
   (test-case "Out of bounds for Y and U"
     (check-equal? (is-YU? 6 6) #f)))) ; Outside bounds for Y and U

;row col -> Boolean
;rules for generating 'I in homepage
(define (is-HU? row col)
  (or
   ;;U shape
   (and
    (= col 8)
    (<= 6 row 10))
   (and
    (= col 12)
    (<= 6 row 10))
   (and
    (= row 10)
    (<= 8 col 12))
   ;;H shape
   (and
    (= col 0)
    (<= 6 row 10))
   (and
    (= col 4)
    (<= 6 row 10))
   (and
    (= row 8)
    (<= 0 col 4))))
;;Test
(define is-HU?-tests
  (test-suite
   "Tests for is-HU?"
   
   ;; U shape tests
   (test-case "U shape: col == 8, 6 <= row <= 10"
     (check-equal? (is-HU? 7 8) #t)) ; col == 8, within row bounds

   (test-case "U shape: col == 12, 6 <= row <= 10"
     (check-equal? (is-HU? 9 12) #t)) ; col == 12, within row bounds

   (test-case "U shape: row == 10, 8 <= col <= 12"
     (check-equal? (is-HU? 10 10) #t)) ; row == 10, within col bounds

   ;; H shape tests
   (test-case "H shape: col == 0, 6 <= row <= 10"
     (check-equal? (is-HU? 8 0) #t)) ; col == 0, within row bounds

   (test-case "H shape: col == 4, 6 <= row <= 10"
     (check-equal? (is-HU? 10 4) #t)) ; col == 4, within row bounds

   (test-case "H shape: row == 8, 0 <= col <= 4"
     (check-equal? (is-HU? 8 3) #t)) ; row == 8, within col bounds

   ;; Negative test cases (should return #f)
   (test-case "Not U or H shape"
     (check-equal? (is-HU? 5 5) #f)) ; Outside U and H shapes
   (test-case "Out of bounds for U and H shapes"
     (check-equal? (is-HU? 11 8) #f)))) ; Outside bounds for both U and H shapes

;row col -> symbol
;rules for generate homepage
(define (homepage-rule row col)
  (cond
    [(is-YU? row col) 'D]
    [(is-HU? row col) 'I]
    [else 'W]))
;;Test
(define homepage-rule-tests
  (test-suite
   "Tests for homepage-rule in grid generation"

   ;; Test 1: Ensure that cells matching the YU shape are assigned 'D
   (test-case "Check YU shape returns 'D"
     (let ((layout (generate-layout 0 '() homepage-rule)))
       (for ([row (in-range MAX-ROWS)])
         (for ([col (in-range MAX-COLS)])
           (let ((cell (vector-ref (vector-ref layout row) col)))
             (if (is-YU? row col)
                 (check-equal? cell 'D)
                 (check-not-equal? cell 'D))))))

   ;; Test 2: Ensure that cells matching the HU shape are assigned 'I
   (test-case "Check HU shape returns 'I"
     (let ((layout (generate-layout 0 '() homepage-rule)))
       (for ([row (in-range MAX-ROWS)])
         (for ([col (in-range MAX-COLS)])
           (let ((cell (vector-ref (vector-ref layout row) col)))
             (if (is-HU? row col)
                 (check-equal? cell 'I)
                 (check-not-equal? cell 'I))))))

   ;; Test 3: Ensure that all other cells are assigned 'W
   (test-case "Check default case returns 'W"
     (let ((layout (generate-layout 0 '() homepage-rule)))
       (for ([row (in-range MAX-ROWS)])
         (for ([col (in-range MAX-COLS)])
           (let ((cell (vector-ref (vector-ref layout row) col)))
             (unless (or (is-YU? row col) (is-HU? row col))
               (check-equal? cell 'W))))))))

;row col -> symbol
;rules for generate random-layout
(define (random-layout-rule row col)
  (cond
    [(is-player1? row col) 'W1D] ;generate the player1 at the top-left corner
    [(is-player2? row col) 'W2U] ;generate the player2 at the top-right corner
    [(is-U? row col) 'I] ;generate the fixed indestructible cell
    [(is-fixed-W? row col) 'W] ;generate the safe starting area :` and .: 
    [else (if (zero? (random 2)) 'D 'W)])) ;generate the random cell, destructible if 0, walkable if 1

;;Test
(define random-layout-rule-tests
  (test-suite
   "Tests for random-layout-rule in grid generation"

   ;; Test 1: Ensure that Player 1's position is correctly assigned 'W1D
   (test-case "Player 1 should be assigned 'W1D"
     (let ((layout (generate-layout 0 '() random-layout-rule)))
       (for ([row (in-range MAX-ROWS)])
         (for ([col (in-range MAX-COLS)])
           (let ((cell (vector-ref (vector-ref layout row) col)))
             (if (is-player1? row col)
                 (check-equal? cell 'W1D)
                 (check-not-equal? cell 'W1D))))))

   ;; Test 2: Ensure that Player 2's position is correctly assigned 'W2U
   (test-case "Player 2 should be assigned 'W2U"
     (let ((layout (generate-layout 0 '() random-layout-rule)))
       (for ([row (in-range MAX-ROWS)])
         (for ([col (in-range MAX-COLS)])
           (let ((cell (vector-ref (vector-ref layout row) col)))
             (if (is-player2? row col)
                 (check-equal? cell 'W2U)
                 (check-not-equal? cell 'W2U))))))

   ;; Test 3: Ensure that indestructible cells are correctly assigned 'I
   (test-case "Indestructible cells should be assigned 'I"
     (let ((layout (generate-layout 0 '() random-layout-rule)))
       (for ([row (in-range MAX-ROWS)])
         (for ([col (in-range MAX-COLS)])
           (let ((cell (vector-ref (vector-ref layout row) col)))
             (if (is-U? row col)
                 (check-equal? cell 'I)
                 (check-not-equal? cell 'I))))))

   ;; Test 4: Ensure that safe starting area cells are assigned 'W
   (test-case "Safe starting area cells should be assigned 'W"
     (let ((layout (generate-layout 0 '() random-layout-rule)))
       (for ([row (in-range MAX-ROWS)])
         (for ([col (in-range MAX-COLS)])
           (let ((cell (vector-ref (vector-ref layout row) col)))
             (if (is-fixed-W? row col)
                 (check-equal? cell 'W)
                 (check-not-equal? cell 'W))))))

   ;; Test 5: Ensure that random cells are either 'D or 'W
   (test-case "Random cells should be assigned 'D or 'W"
     (let ((layout (generate-layout 0 '() random-layout-rule)))
       (for ([row (in-range MAX-ROWS)])
         (for ([col (in-range MAX-COLS)])
           (let ((cell (vector-ref (vector-ref layout row) col)))
             (if (not (or (is-player1? row col) (is-player2? row col) (is-U? row col) (is-fixed-W? row col)))
                 (check-true (or (eq? cell 'D) (eq? cell 'W)))))))))

;row col acc rule-fn -> vector of symbol
;generate row
(define (generate-row row col acc rule-fn)
  (if (= col MAX-COLS) ;if one row comes to the end
      (list->vector (reverse acc)) ;transform the acc list to vector
      (generate-row ;else
       row ;the row don't change
       (+ col 1) ;move to the next cell on the same row
       (cons (rule-fn row col) acc) ;new accumulator
       rule-fn)))
;;Test---missing---

;row acc rule-fn -> vector of vector of symbol
;generate layout
(define (generate-layout row acc rule-fn)
  (if (= row MAX-ROWS) ;if all rows come the end
      (list->vector (reverse acc))  ;tranform the list to vector
      (generate-layout ;else
       (+ row 1) ;change to the next row
       (cons (generate-row row 0 '() rule-fn) acc) ;column starts from 0, acc starts from '()
       rule-fn)))
;;Test---missing---

;homepage and random-layout
(define homepage (generate-layout 0 '() homepage-rule))
(define random-layout (generate-layout 0 '() random-layout-rule))



;constant definitions
(define CELL-SIZE (image-width (bitmap "W.png"))) ;the width of the walkable cell image

(define player1-image-D (bitmap "P1D.png"))
(define player1-image-U (bitmap "P1U.png"))
(define player1-image-L (bitmap "P1L.png"))
(define player1-image-R (bitmap "P1R.png"))

(define player2-image-D (bitmap "P2D.png"))
(define player2-image-U (bitmap "P2U.png"))
(define player2-image-L (bitmap "P2L.png"))
(define player2-image-R (bitmap "P2R.png"))

(define SPACE (square 30 0 "white"))

; render-cell
; symbol -> Image
(define (render-cell symbol)
  (crop 0 0 CELL-SIZE CELL-SIZE
        (cond
          [(symbol=? symbol 'D) (bitmap "D.png")]
          [(symbol=? symbol 'I) (bitmap "U.png")]
          [(symbol=? symbol 'W) (bitmap "W.png")]
          [(symbol=? symbol 'B) (bitmap "B.png")]
          [(symbol=? symbol 'E2) (bitmap "T.png")]
          [(symbol=? symbol 'E1) (bitmap "T.png")]
          [(symbol=? symbol 'E0) (bitmap "T.png")]
               
          

          ;player1
          [(symbol=? symbol 'W1L) (overlay player1-image-L (bitmap "W.png"))]
          [(symbol=? symbol 'W1R) (overlay player1-image-R (bitmap "W.png"))]
          [(symbol=? symbol 'W1U) (overlay player1-image-U (bitmap "W.png"))]
          [(symbol=? symbol 'W1D) (overlay player1-image-D (bitmap "W.png"))]
          [(symbol=? symbol 'B1L) (overlay player1-image-L (bitmap "B.png"))]
          [(symbol=? symbol 'B1R) (overlay player1-image-R (bitmap "B.png"))]
          [(symbol=? symbol 'B1U) (overlay player1-image-U (bitmap "B.png"))]
          [(symbol=? symbol 'B1D) (overlay player1-image-D (bitmap "B.png"))]
  

          ;player2
          [(symbol=? symbol 'W2L) (overlay player2-image-L (bitmap "W.png"))]
          [(symbol=? symbol 'W2R) (overlay player2-image-R (bitmap "W.png"))]
          [(symbol=? symbol 'W2U) (overlay player2-image-U (bitmap "W.png"))]
          [(symbol=? symbol 'W2D) (overlay player2-image-D (bitmap "W.png"))]
          [(symbol=? symbol 'B2L) (overlay player2-image-L (bitmap "B.png"))]
          [(symbol=? symbol 'B2R) (overlay player2-image-R (bitmap "B.png"))]
          [(symbol=? symbol 'B2U) (overlay player2-image-U (bitmap "B.png"))]
          [(symbol=? symbol 'B2D) (overlay player2-image-D (bitmap "B.png"))]

          ;player1-dead-image

          [(symbol=? symbol 'E1D) (overlay player1-image-D (bitmap "Boom.png"))]
          [(symbol=? symbol 'E1U) (overlay player1-image-U (bitmap "Boom.png"))]
          [(symbol=? symbol 'E1L) (overlay player1-image-L (bitmap "Boom.png"))]
          [(symbol=? symbol 'E1R) (overlay player1-image-R (bitmap "Boom.png"))]

          ;player2-dead-image

          [(symbol=? symbol 'E2D) (overlay player2-image-D (bitmap "Boom.png"))]
          [(symbol=? symbol 'E2U) (overlay player2-image-U (bitmap "Boom.png"))]
          [(symbol=? symbol 'E2L) (overlay player2-image-L (bitmap "Boom.png"))]
          [(symbol=? symbol 'E2R) (overlay player2-image-R (bitmap "Boom.png"))])))
;;Test
(define (bitmap path) path) ; return the path for the bitmap
(define (overlay img1 img2) (list img1 img2)) ; return a list representing the overlay for testing
(define render-cell-tests
  (test-suite
   "Tests for render-cell function"
   
   ;; Test 1: Check 'D' symbol renders the correct bitmap
   (test-case "Check 'D' symbol renders 'D.png'"
     (check-equal? (render-cell 'D) "D.png"))
   
   ;; Test 2: Check 'I' symbol renders 'U.png'
   (test-case "Check 'I' symbol renders 'U.png'"
     (check-equal? (render-cell 'I) "U.png"))
   
   ;; Test 3: Check 'W' symbol renders 'W.png'
   (test-case "Check 'W' symbol renders 'W.png'"
     (check-equal? (render-cell 'W) "W.png"))
   
   ;; Test 4: Check 'B' symbol renders 'B.png'
   (test-case "Check 'B' symbol renders 'B.png'"
     (check-equal? (render-cell 'B) "B.png"))
   
   ;; Test 5: Check player1 'W1D' renders overlay with player1 image and 'W.png'
   (test-case "Check player1 'W1D' symbol renders player1 image with 'W.png'"
     (check-equal? (render-cell 'W1D) (list player1-image-D "W.png")))
   
   ;; Test 6: Check player2 'W2U' renders overlay with player2 image and 'W.png'
   (test-case "Check player2 'W2U' symbol renders player2 image with 'W.png'"
     (check-equal? (render-cell 'W2U) (list player2-image-U "W.png")))

   ;; Test 7: Check dead player1 'E1D' renders overlay with player1 image and 'Boom.png'
   (test-case "Check dead player1 'E1D' symbol renders player1 image with 'Boom.png'"
     (check-equal? (render-cell 'E1D) (list player1-image-D "Boom.png")))

   ;; Test 8: Check dead player2 'E2D' renders overlay with player2 image and 'Boom.png'
   (test-case "Check dead player2 'E2D' symbol renders player2 image with 'Boom.png'"
     (check-equal? (render-cell 'E2D) (list player2-image-D "Boom.png")))))

; render-row:
(define (render-row layout)
  (let loop ( ;function name loop
             [i 0] ;start: i=0
             [acc empty-image] ;start: acc=empty-image
             )
    (if (>= i MAX-COLS)
        acc
        (loop (+ i 1)
              (beside acc (render-cell (vector-ref layout i)))))))        
;;Test---missing---

; render-layout
; layout -> Image
(define (render-layout layout)
  (let loop (
             [i 0]
             [acc empty-image]
             )
    (if (>= i MAX-ROWS)
         acc
        (loop (+ i 1) 
              (above acc (render-row (vector-ref layout i)))))))
;;Test---missing---

;convert-seconds-to-minutes-and-seconds-string:
;Number -> String
(define (seconds->minutes-and-seconds-string seconds)
  (let* (
        [minutes (quotient seconds 60)]
        [remaining-seconds (remainder seconds 60)]
        [length-remaining-seconds (string-length
                                   (number->string remaining-seconds))]
        )
  (string-append
   (number->string minutes)
   ":"
   (cond
     [(< length-remaining-seconds 2) (string-append "0" (number->string remaining-seconds))]
     [else (number->string remaining-seconds)]))))                   
;;Test
(define (test-seconds->minutes-and-seconds-string)
  (test-suite
   "Testing seconds->minutes-and-seconds-string"
   
   ;; Test case 1: 0 seconds -> "0:00"
   (test-case "0 seconds"
     (check-equal? (seconds->minutes-and-seconds-string 0) "0:00"))
   
   ;; Test case 2: 65 seconds -> "1:05"
   (test-case "65 seconds"
     (check-equal? (seconds->minutes-and-seconds-string 65) "1:05"))
   
   ;; Test case 3: 125 seconds -> "2:05"
   (test-case "125 seconds"
     (check-equal? (seconds->minutes-and-seconds-string 125) "2:05"))
   
   ;; Test case 4: 3600 seconds -> "60:00"
   (test-case "3600 seconds"
     (check-equal? (seconds->minutes-and-seconds-string 3600) "60:00"))
   
   ;; Test case 5: 61 seconds -> "1:01"
   (test-case "61 seconds"
     (check-equal? (seconds->minutes-and-seconds-string 61) "1:01"))
   
   ;; Test case 6: 59 seconds -> "0:59"
   (test-case "59 seconds"
     (check-equal? (seconds->minutes-and-seconds-string 59) "0:59"))))

;render-bar
;roundtimer maximum owner1 owner2 -> Image
(define (render-bar roundtimer maximum owner1 owner2)
  (beside
   (text (string-append "P1: " (number->string owner1)) 30 "indigo")
   SPACE
   (text (string-append "Roundtimer: " (seconds->minutes-and-seconds-string roundtimer)) 30 "indigo")
   SPACE
   (text (string-append "Maximum bomb: " (number->string maximum)) 30 "indigo")
   SPACE
   (text (string-append "P2: " (number->string owner2)) 30 "indigo")))
;;Test
(define (test-render-bar)
  (test-suite
   "Testing render-bar"
   
   ;; Test case 1: render-bar with typical values
   (test-case "render-bar with typical values"
     (check-equal? (render-bar 120 5 50 30)
                   (beside
                    (text "P1: 50" 30 "indigo")
                    SPACE
                    (text "Roundtimer: 2:00" 30 "indigo")
                    SPACE
                    (text "Maximum bomb: 5" 30 "indigo")
                    SPACE
                    (text "P2: 30" 30 "indigo"))))
   
   ;; Test case 2: render-bar with edge values (e.g., 0 roundtimer)
   (test-case "render-bar with edge values"
     (check-equal? (render-bar 0 5 10 15)
                   (beside
                    (text "P1: 10" 30 "indigo")
                    SPACE
                    (text "Roundtimer: 0:00" 30 "indigo")
                    SPACE
                    (text "Maximum bomb: 5" 30 "indigo")
                    SPACE
                    (text "P2: 15" 30 "indigo"))))

   ;; Test case 3: render-bar with large values
   (test-case "render-bar with large values"
     (check-equal? (render-bar 3600 100 999 888)
                   (beside
                    (text "P1: 999" 30 "indigo")
                    SPACE
                    (text "Roundtimer: 60:00" 30 "indigo")
                    SPACE
                    (text "Maximum bomb: 100" 30 "indigo")
                    SPACE
                    (text "P2: 888" 30 "indigo"))))))

;count-num
(define (count-num list-of-bomb)
  (cond
    [(empty? list-of-bomb) 0]
    [else
     (add1 (count-num (rest list-of-bomb)))]))
;;Test
(define (test-count-num)
  (test-suite
   "Testing count-num"
   
   ;; Test case 1: Count elements in a non-empty list
   (test-case "counting elements in a list"
     (check-equal? (count-num '(W1D W2U W1U)) 3))
   
   ;; Test case 2: Empty list should return 0
   (test-case "counting elements in an empty list"
     (check-equal? (count-num '()) 0))
   
   ;; Test case 3: List with one element should return 1
   (test-case "counting one element"
     (check-equal? (count-num '(W1D)) 1))
   
   ;; Test case 4: List with repeated elements
   (test-case "counting repeated elements"
     (check-equal? (count-num '(W1D W1D W1D)) 3))))
 
;homepage-bar     
(define homepage-bar
   (text "Enter space to start game"
        30
        "indigo"))


;render
;gamestate -> Image
(define (render gamestate)
  (let (
        [layout (gamestate-layout gamestate)]
        )
    (cond
      [(equal? layout homepage)
       (above
        homepage-bar
        (render-layout homepage))] ;layout of homepage
      [else
    
  (let* ( ;layout of game page
        [roundtimer (gamestate-roundtimer gamestate)]
        [maximum (gamestate-maximum gamestate)]
        [bomb-list (gamestate-bomb gamestate)]
        [owner1 (count-num (filter
                            (lambda(bombstate)
                              (symbol=? (bombstate-owner bombstate) 'P1))
                           bomb-list))]
        [owner2 (count-num (filter
                            (lambda(bombstate)
                              (symbol=? (bombstate-owner bombstate) 'P2))
                            bomb-list))]
        )
    (above (render-bar roundtimer maximum owner1 owner2)
           (render-layout (gamestate-layout gamestate))))])))

;;Test---missing---






