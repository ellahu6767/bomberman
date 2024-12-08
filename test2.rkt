;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname test2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;----------render----------;;
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
                 (check-not-equal? cell 'D)))))))

   ;; Test 2: Ensure that cells matching the HU shape are assigned 'I
   (test-case "Check HU shape returns 'I"
     (let ((layout (generate-layout 0 '() homepage-rule)))
       (for ([row (in-range MAX-ROWS)])
         (for ([col (in-range MAX-COLS)])
           (let ((cell (vector-ref (vector-ref layout row) col)))
             (if (is-HU? row col)
                 (check-equal? cell 'I)
                 (check-not-equal? cell 'I)))))))

   ;; Test 3: Ensure that all other cells are assigned 'W
   (test-case "Check default case returns 'W"
     (let ((layout (generate-layout 0 '() homepage-rule)))
       (for ([row (in-range MAX-ROWS)])
         (for ([col (in-range MAX-COLS)])
           (let ((cell (vector-ref (vector-ref layout row) col)))
             (unless (or (is-YU? row col) (is-HU? row col))
               (check-equal? cell 'W)))))))))

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
                 (check-not-equal? cell 'W1D)))))))

   ;; Test 2: Ensure that Player 2's position is correctly assigned 'W2U
   (test-case "Player 2 should be assigned 'W2U"
     (let ((layout (generate-layout 0 '() random-layout-rule)))
       (for ([row (in-range MAX-ROWS)])
         (for ([col (in-range MAX-COLS)])
           (let ((cell (vector-ref (vector-ref layout row) col)))
             (if (is-player2? row col)
                 (check-equal? cell 'W2U)
                 (check-not-equal? cell 'W2U)))))))

   ;; Test 3: Ensure that indestructible cells are correctly assigned 'I
   (test-case "Indestructible cells should be assigned 'I"
     (let ((layout (generate-layout 0 '() random-layout-rule)))
       (for ([row (in-range MAX-ROWS)])
         (for ([col (in-range MAX-COLS)])
           (let ((cell (vector-ref (vector-ref layout row) col)))
             (if (is-U? row col)
                 (check-equal? cell 'I)
                 (check-not-equal? cell 'I)))))))

   ;; Test 4: Ensure that safe starting area cells are assigned 'W
   (test-case "Safe starting area cells should be assigned 'W"
     (let ((layout (generate-layout 0 '() random-layout-rule)))
       (for ([row (in-range MAX-ROWS)])
         (for ([col (in-range MAX-COLS)])
           (let ((cell (vector-ref (vector-ref layout row) col)))
             (if (is-fixed-W? row col)
                 (check-equal? cell 'W)
                 (check-not-equal? cell 'W)))))))

   ;; Test 5: Ensure that random cells are either 'D or 'W
   (test-case "Random cells should be assigned 'D or 'W"
     (let ((layout (generate-layout 0 '() random-layout-rule)))
       (for ([row (in-range MAX-ROWS)])
         (for ([col (in-range MAX-COLS)])
           (let ((cell (vector-ref (vector-ref layout row) col)))
             (if (not (or (is-player1? row col) (is-player2? row col) (is-U? row col) (is-fixed-W? row col)))
                 (check-true (or (eq? cell 'D) (eq? cell 'W)))))))))))

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

;;--------on_key---------;;
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




