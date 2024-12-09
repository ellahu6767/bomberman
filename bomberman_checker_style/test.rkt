;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/base)
(require "public.rkt")
(require "on_tick.rkt")
(require "stop_when.rkt")
(require rackunit)
(require rackunit/text-ui)

;;constant definitions
;;To prevent the vector from being modified during testing
;;all example layouts are defined
;;using let or let* within specific tests or test-case.
(define example-bomblist-with-zero-countdown
  (list (make-bombstate (make-cor 1 1) 3 'P1) 
        (make-bombstate (make-cor 2 2) 0 'P2) 
        (make-bombstate (make-cor 3 3) 2 'P1)
        (make-bombstate (make-cor 4 4) 0 'P2)))

(define example-bomblist-without-zero-countdown
  (list (make-bombstate (make-cor 1 1) 3 'P1) 
        (make-bombstate (make-cor 2 2) 2 'P2) 
        (make-bombstate (make-cor 3 3) 2 'P1)
        (make-bombstate (make-cor 4 4) 1 'P2)))

(define example-bomblist-with-all-zero-countdown
  (list (make-bombstate (make-cor 1 1) 0 'P1)
        (make-bombstate (make-cor 3 3) 0 'P2)))

(define center-cor (make-cor 1 1))
(define northwest-corner-cor (make-cor 0 0))
(define southeast-corner-cor (make-cor 2 2))
(define northeast-corner-cor (make-cor 2 0))
(define southwest-corner-cor (make-cor 0 2))


;remove-bomb-test
(define remove-bomb-tests
  (test-suite "remove bomb tests"
              
    ;; some bombs countdown = 0
    (test-case "remove bombs with countdown = 0"
               (check-equal?
                (remove-bomb example-bomblist-with-zero-countdown)
                (list (make-bombstate (make-cor 1 1) 3 'P1)
                      (make-bombstate (make-cor 3 3) 2 'P1))))

    ;; all bombs countdown > 0
    (test-case "no bombs to remove"
               (check-equal?
                (remove-bomb example-bomblist-without-zero-countdown)
                example-bomblist-without-zero-countdown))

    ;; no bombs
    (test-case "empty bomb list"
               (check-equal? (remove-bomb '()) '()))

    ;; all bombs countdown = 0
    (test-case "all bombs have countdown = 0"
               (check-equal? (remove-bomb example-bomblist-with-all-zero-countdown) '()))
    ))


;check-zero?-test
(define check-zero-tests
  (test-suite "check zero tests"
              (test-case "bomb list has countdown = 0"
                         (check-true (check-zero? example-bomblist-with-zero-countdown)))
              (test-case "bomb list without countdown = 0"
                         (check-false (check-zero? example-bomblist-without-zero-countdown)))
              (test-case "empty bomb list"
                         (check-false (check-zero? '())))))


;update-bomb-test
(define updated-bomb-tests
  (test-suite "updated bomb tests"
              (test-case "updated bombs with countdown > 0"
                         (check-equal?
                          (updated-bomb example-bomblist-with-zero-countdown)
                          (list (make-bombstate (make-cor 1 1) 2 'P1)
                                (make-bombstate (make-cor 2 2) 0 'P2)
                                (make-bombstate (make-cor 3 3) 1 'P1)
                                (make-bombstate (make-cor 4 4) 0 'P2))))
              (test-case "empty bomb list"
                         (check-equal?
                          (updated-bomb '())
                          '()))
              ))

;updated-timer
(define updated-roundtimer-tests
  (test-suite "updated roundtimer tests"
              (test-case "decrease roundtimer"
                         (check-equal? (updated-roundtimer 10) 9))
              (test-case "roundtimer stays at 0"
                         (check-equal? (updated-roundtimer 0) 0))))


;check-roundtimer?
(define check-roundtimer?-tests
  (test-suite "check roundtimer tests"
              (test-case "roundtimer is divisible by 30"
                         (check-true (check-roundtimer? 30)))
              (test-case "roundtimer not divisible by 30"
                         (check-false (check-roundtimer? 31)))
              (test-case "initial-roundtimer"
                         (check-false (check-roundtimer? initial-roundtimer)))
              (test-case "roundtimer=0"
                         (check-false (check-roundtimer? 0)))))

; add-maximum 
(define add-maximum-tests
  (test-suite "add maximum tests"
              (test-case "increase maximum"
                         (check-equal? (add-maximum 5) 6))
              (test-case "maximum does not exceed limit"
                         (check-equal? (add-maximum 6) 6))))


;; Updated E-Symbol Tests
(define updated-E-symbol-tests
  (test-suite "update E symbol tests"
    (test-case "E1 to E0"
      (check-equal? (update-E-symbols 'E1) 'E0))
    (test-case "E0 to W"
      (check-equal? (update-E-symbols 'E0) 'W))
    (test-case "W remains W"
      (check-equal? (update-E-symbols 'W) 'W))))

;; Updated Layout Tests
(define updated-layout-tests
  (test-suite "updated layout tests"   
    (test-case "E1 to E0 layout"
               (let (
                     [layout-many-E1
                     (vector
                      (vector 'E1 'E1 'E1)
                      (vector 'E1 'E1 'E1)
                      (vector 'E1 'E1 'W))]
                     [layout-many-E0
                      (vector
                       (vector 'E0 'E0 'E0)
                       (vector 'E0 'E0 'E0)
                       (vector 'E0 'E0 'W))]
                     )
      (check-equal? (updated-layout layout-many-E1) layout-many-E0)))
    
    (test-case "E0 to W layout"
               (let (
                     [layout-many-E0
                      (vector
                       (vector 'E0 'E0 'E0)
                       (vector 'E0 'E0 'E0)
                       (vector 'E0 'E0 'W))]
                     [layout-all-W
                      (vector
                       (vector 'W 'W 'W)
                       (vector 'W 'W 'W)
                       (vector 'W 'W 'W))]
                     )          
      (check-equal? (updated-layout layout-many-E0) layout-all-W)))
    (test-case "no E, no change"
               (let (
                     [layout-all-W
                      (vector
                       (vector 'W 'W 'W)
                       (vector 'W 'W 'W)
                       (vector 'W 'W 'W))]
                     )
      (check-equal? (updated-layout layout-all-W) layout-all-W)))))
               



;extend-direction
(define extend-direction-tests
  (let (
        [layout-all-W
  (vector
   (vector 'W 'W 'W)
   (vector 'W 'W 'W)
   (vector 'W 'W 'W))]
        [layout-in-many-D
  (vector
   (vector 'W 'D 'D)
   (vector 'D 'W 'D)
   (vector 'D 'D 'D))]
        [layout-in-many-I
  (vector
   (vector 'I 'I 'I)
   (vector 'I 'W 'I)
   (vector 'I 'I 'W))]
        [layout-in-mix-D-I
  (vector
   (vector 'I 'D 'W)
   (vector 'D 'W 'I)
   (vector 'I 'I 'I))]
        )
  (test-suite "extend direction tests"
              (test-case "extend without any obstacle"
                         (let ([potential-list (list (make-cor 1 0) (make-cor 2 0))])
                           (check-equal? (extend-direction potential-list layout-all-W)
                                         (list (make-cor 1 0) (make-cor 2 0))))) 

    (test-case "dismental 'D but stop extending"
               (let ([potential-list (list (make-cor 1 0) (make-cor 2 0))])
                 (check-equal? (extend-direction potential-list layout-in-many-D)
                               (list (make-cor 1 0))))) 

    (test-case "blocked by 'I obstacle"
               (let ([potential-list (list (make-cor 1 0) (make-cor 2 0))])
                 (check-equal? (extend-direction potential-list layout-in-many-I)
                               '()))) 

    (test-case "out of bounds"
               (let ([potential-list (list (make-cor -1 0) (make-cor -2 0))])
                 (check-equal? (extend-direction potential-list layout-all-W)
                               '())))
  )))


;;single-boom-range-tests
(define single-boom-range-tests
  (let (
        [layout-all-W
  (vector
   (vector 'W 'W 'W)
   (vector 'W 'W 'W)
   (vector 'W 'W 'W))]
        [layout-in-many-D
  (vector
   (vector 'W 'D 'D)
   (vector 'D 'W 'D)
   (vector 'D 'D 'D))]
        [layout-in-many-I
  (vector
   (vector 'I 'I 'I)
   (vector 'I 'W 'I)
   (vector 'I 'I 'W))]
        [layout-in-mix-D-I
  (vector
   (vector 'I 'D 'W)
   (vector 'D 'W 'I)
   (vector 'I 'I 'I))]
        )
  (test-suite "single boom range tests"
    ;; layout-all-W
    (test-case "all W layout: center bomb only"
      (let ([bomb (make-bombstate center-cor 0 'P1)])
        (check-equal? (single-boom-range bomb layout-all-W)
                      (list (make-cor 1 1)
                            (make-cor 1 0)
                            (make-cor 1 2)
                            (make-cor 0 1)
                            (make-cor 2 1))))) 

    ;; layout-in-many-D
    (test-case "many D layout: center bomb only"
      (let ([bomb (make-bombstate center-cor 0 'P1)])
        (check-equal? (single-boom-range bomb layout-in-many-D)
                      (list (make-cor 1 1)
                            (make-cor 1 0)
                            (make-cor 1 2) 
                            (make-cor 0 1)
                            (make-cor 2 1))))) 

    ;; layout-in-many-I
    (test-case "many I layout: center bomb only"
      (let ([bomb (make-bombstate center-cor 0 'P1)])
        (check-equal? (single-boom-range bomb layout-in-many-I)
                      (list (make-cor 1 1))))) 

    ;; layout-in-mix-D-I
    (test-case "mix D and I layout: center bomb only"
      (let ([bomb (make-bombstate center-cor 0 'P1)])
        (check-equal? (single-boom-range bomb layout-in-mix-D-I)
                      (list (make-cor 1 1)
                            (make-cor 1 0) 
                            (make-cor 0 1)))))))) 
  


;;boom-range-tests
(define boom-range-tests
  (let (
        [layout-all-W
  (vector
   (vector 'W 'W 'W)
   (vector 'W 'W 'W)
   (vector 'W 'W 'W))]
        [layout-in-many-D
  (vector
   (vector 'W 'D 'D)
   (vector 'D 'W 'D)
   (vector 'D 'D 'D))]
        [layout-in-many-I
  (vector
   (vector 'I 'I 'I)
   (vector 'I 'W 'I)
   (vector 'I 'I 'W))]
        [layout-in-mix-D-I
  (vector
   (vector 'I 'D 'W)
   (vector 'D 'W 'I)
   (vector 'I 'I 'I))]
        )
  (test-suite "boom range tests"
    ;; layout-all-W
    (test-case "all W layout: center and southwest corner bombs"
      (let (
            [bomb-list (list (make-bombstate center-cor 0 'P1)
                             (make-bombstate southwest-corner-cor 0 'P2))]
            )
        (check-equal? (boom-range bomb-list layout-all-W)
                      (list (make-cor 1 1)
                            (make-cor 1 0)
                            (make-cor 1 2)
                            (make-cor 0 1)
                            (make-cor 2 1)
                            ;;center bomb
                            (make-cor 0 2)
                            (make-cor 0 1)
                            (make-cor 0 0)
                            (make-cor 1 2)
                            (make-cor 2 2)
                            ;southwest bomb
                            ))))
                            
    ;; layout-in-many-D
    (test-case "many D layout: center and northwest corner bombs"
      (let ([bomb-list (list (make-bombstate center-cor 0 'P1)
                             (make-bombstate northwest-corner-cor 0 'P2))])
        (check-equal? (boom-range bomb-list layout-in-many-D)
                      (list (make-cor 1 1)
                            (make-cor 1 0)
                            (make-cor 1 2) 
                            (make-cor 0 1)
                            (make-cor 2 1)
                            ;center bomb
                            (make-cor 0 0)
                            (make-cor 0 1)
                            (make-cor 1 0)
                            ;northwest bomb
                            ))))
    

    ;; layout-in-many-I
    (test-case "many I layout: center and southeast corner bombs"
      (let ([bomb-list (list (make-bombstate center-cor 0 'P1)
                             (make-bombstate southeast-corner-cor 0 'P2))])
        (check-equal? (boom-range bomb-list layout-in-many-I)
                      (list (make-cor 1 1) ; center bomb
                            (make-cor 2 2) ; southeast corner bomb
                            )))) 

    ;; layout-in-mix-D-I
    (test-case "mix D and I layout: center and northeast corner bombs"
      (let ([bomb-list (list (make-bombstate center-cor 0 'P1)
                             (make-bombstate northeast-corner-cor 0 'P2))])
        (check-equal? (boom-range bomb-list layout-in-mix-D-I)
                      (list (make-cor 1 1)
                            (make-cor 1 0) 
                            (make-cor 0 1)
                            ;center bomb
                            (make-cor 2 0)
                            (make-cor 1 0)
                            ;northeast bomb
                            ))))
  )))


;chain-explosion-test
(define chain-explosion-tests
  (let (
        [initial-boom-cor (list 
                                (make-cor 1 1) 
                                (make-cor 0 1) 
                                (make-cor 2 1) 
                                (make-cor 1 0)
                                (make-cor 1 2))]
        )
  (test-suite "chain Explosion Tests"
    (test-case "chain explosion"
      (let (
             [bomb-list (list (make-bombstate (make-cor 1 1) 0 'P1) 
                              (make-bombstate (make-cor 1 2) 3 'P2)
                              (make-bombstate (make-cor 0 1) 2 'P1)
                              (make-bombstate (make-cor 1 0) 1 'P1))]
             )
        (check-equal? (chain-explosion initial-boom-cor bomb-list)
                      (list (make-bombstate (make-cor 1 1) 0 'P1) 
                            (make-bombstate (make-cor 1 2) 0 'P2)
                            (make-bombstate (make-cor 0 1) 0 'P1)
                            (make-bombstate (make-cor 1 0) 0 'P1)))))
    

   
    (test-case "no chain explosion"
      (let (
             [bomb-list (list (make-bombstate (make-cor 1 1) 0 'P1)
                              (make-bombstate (make-cor 2 2) 3 'P2))]
             )
        (check-equal? (chain-explosion initial-boom-cor bomb-list)
                      (list (make-bombstate (make-cor 1 1) 0 'P1) 
                            (make-bombstate (make-cor 2 2) 3 'P2))))) 


    (test-case "bomb already at countdown 0 is not affected"
      (let (
             [bomb-list (list (make-bombstate (make-cor 1 1) 0 'P1) 
                              (make-bombstate (make-cor 1 2) 0 'P2))]
             )
        (check-equal? (chain-explosion initial-boom-cor bomb-list)
                      (list (make-bombstate (make-cor 1 1) 0 'P1) 
                            (make-bombstate (make-cor 1 2) 0 'P2)))))) 
  ))


;; cor=? Tests
(define cor=?-tests
  (test-suite "cor=? tests"
    (test-case "equal coordinates"
      (check-true (cor=? (make-cor 1 1) (make-cor 1 1))))
    (test-case "different column"
      (check-false (cor=? (make-cor 1 1) (make-cor 1 2))))
    (test-case "different row"
      (check-false (cor=? (make-cor 1 1) (make-cor 2 1))))
    (test-case "completely different"
      (check-false (cor=? (make-cor 1 1) (make-cor 2 2))))))

;; in-boom-cor? Tests
(define in-boom-cor?-tests
  (let (
        [cor-list (list (make-cor 1 1) (make-cor 1 2) (make-cor 1 0))]
        )
    (test-suite "in-boom-cor? tests"
      (test-case "in list: first element"
        (check-true (in-boom-cor? (make-cor 1 1) cor-list)))
      (test-case "in list: second element"
        (check-true (in-boom-cor? (make-cor 1 2) cor-list)))
      (test-case "in list: third element"
        (check-true (in-boom-cor? (make-cor 1 0) cor-list)))
      (test-case "not in list: out of range"
        (check-false (in-boom-cor? (make-cor 4 4) cor-list)))
      (test-case "empty list"
        (check-false (in-boom-cor? (make-cor 1 1) '()))))))


;; get-symbol Tests
(define get-symbol-tests
  (let (
        [layout-mix-symbol
         (vector
          (vector 'W 'W1U 'I)
          (vector 'D 'B 'E1)
          (vector 'E1 'B2L 'E2U))]
        )
  (test-suite "get-symbol Tests"
    (test-case "valid position"   
      (check-equal? (get-symbol layout-mix-symbol (make-cor 1 1)) 'B))
    (test-case "out of bounds"
      (check-equal? (get-symbol layout-mix-symbol (make-cor 3 3)) 'ILEGAL)) 
    (test-case "valid position"
      (check-equal? (get-symbol layout-mix-symbol (make-cor 0 1)) 'D))
    (test-case "valid position"
      (check-equal? (get-symbol layout-mix-symbol (make-cor 0 0)) 'W)))))


;; check-I? Tests
(define check-I?-tests
  (let (
       [layout-many-I
  (vector
   (vector 'I 'I 'I)
   (vector 'I 'W 'I)
   (vector 'I 'I 'W))]
       )
  (test-suite "check-I? tests"
    (test-case "valid I position"
      (check-true (check-I? layout-many-I (make-cor 0 0))))
    (test-case "valid I position"
      (check-true (check-I? layout-many-I (make-cor 1 2))))
    (test-case "not I"
      (check-false (check-I? layout-many-I (make-cor 1 1))))
    (test-case "out of bounds"
      (check-false (check-I? layout-many-I (make-cor 3 0)))))))

;; check-D? Tests
(define check-D?-tests
  (let(
       [layout-many-D
  (vector
   (vector 'D 'D 'D)
   (vector 'D 'W 'D)
   (vector 'D 'D 'W))]
       ) 
  (test-suite "check-D? tests"
    (test-case "valid D position"
      (check-true (check-D? layout-many-D (make-cor 0 1))))
    (test-case "valid D position"
      (check-true (check-D? layout-many-D (make-cor 0 0))))
    (test-case "not D"
      (check-false (check-D? layout-many-D (make-cor 1 1))))
    (test-case "out of bounds"
      (check-false (check-D? layout-many-D (make-cor 3 3)))))))

;; convert Tests
(define convert-tests
  (let (
        [layout-all-W
         (vector
   (vector 'W 'W 'W)
   (vector 'W 'W 'W)
   (vector 'W 'W 'W))]
        )
  (test-suite "convert tests"
    (test-case "convert some positions to E1"
      (let ([converted-layout
             (convert layout-all-W (list (make-cor 1 1) (make-cor 0 0)) 'E1)])
        (check-equal? (get-symbol converted-layout (make-cor 1 1)) 'E1)
        (check-equal? (get-symbol converted-layout (make-cor 0 0)) 'E1)
        (check-equal? (get-symbol converted-layout (make-cor 2 2)) 'W))))))

;;boom
;;since boom-range has tested in single-boom-range and boom-range tests
;;so in boom function, use layout-all-W as layout(define boom-tests
(define boom-tests
  (test-suite "boom function tests"
    (test-case "no bombs: players and layout remain unchanged"
      (let* (
             [layout-all-W
              (vector
               (vector 'W 'W 'W)
               (vector 'W 'W 'W)
               (vector 'W 'W 'W))]
             
             [bomb-list '()]
             [player1 (make-player1 (make-cor 0 0) "U")]
             [player2 (make-player2 (make-cor 2 2) "D")]
             [gamestate (make-gamestate layout-all-W bomb-list player1 player2 120 6 #f)]
             [result (boom gamestate)])
        (check-equal? (gamestate-layout result) layout-all-W)
        (check-equal? (gamestate-player1 result) player1)
        (check-equal? (gamestate-player2 result) player2)))
    (test-case "single bomb, players out of range"
      (let* (
             [layout-all-W
              (vector
               (vector 'W 'W 'W)
               (vector 'W 'W 'W)
               (vector 'W 'W 'W))]
             [bomb-list (list (make-bombstate (make-cor 1 1) 0 'P1))]
             [player1 (make-player1 (make-cor 0 0) "U")]
             [player2 (make-player2 (make-cor 2 2) "D")]
             [gamestate (make-gamestate layout-all-W bomb-list player1 player2 120 6 #f)]
             [result (boom gamestate)]
             [expected-layout (convert layout-all-W (list (make-cor 1 1) 
                                                            (make-cor 0 1) 
                                                            (make-cor 2 1) 
                                                            (make-cor 1 0) 
                                                            (make-cor 1 2)) 'E1)])
        (check-equal? (gamestate-layout result) expected-layout)
        (check-equal? (gamestate-player1 result) player1)
        (check-equal? (gamestate-player2 result) player2)))
    (test-case "single bomb with player1 in range"
      (let* (
             [layout-all-W
              (vector
               (vector 'W 'W 'W)
               (vector 'W 'W 'W)
               (vector 'W 'W 'W))]
             [bomb-list (list (make-bombstate (make-cor 1 1) 0 'P1))]
             [player1 (make-player1 (make-cor 1 1) "U")]
             [player2 (make-player2 (make-cor 2 2) "D")]
             [gamestate (make-gamestate layout-all-W bomb-list player1 player2 120 6 #f)]
             [result (boom gamestate)]
             [expected-layout (convert (convert layout-all-W
                                                (list (make-cor 0 1) 
                                                      (make-cor 2 1) 
                                                      (make-cor 1 0) 
                                                      (make-cor 1 2)) 'E1)
                                       (make-cor 1 1) 'E1U)])
        (check-equal? (gamestate-layout result) expected-layout)
        (check-equal? (gamestate-player1 result)
                      (make-player1 (make-cor 1 1) "U"))
        (check-equal? (gamestate-player2 result) player2)))
    (test-case "both players in explosion range"
      (let* (
             [layout-all-W
              (vector
               (vector 'W 'W 'W)
               (vector 'W 'W 'W)
               (vector 'W 'W 'W))]
             [bomb-list (list (make-bombstate (make-cor 1 1) 0 'P1))]
             [player1 (make-player1 (make-cor 1 1) "U")]
             [player2 (make-player2 (make-cor 1 2) "L")]
             [gamestate (make-gamestate layout-all-W bomb-list player1 player2 120 6 #f)]
             [result (boom gamestate)]
             [expected-layout (convert (convert (convert layout-all-W
                                                         (list (make-cor 0 1)
                                                               (make-cor 2 1)
                                                               (make-cor 1 0)) 'E1)
                                                (make-cor 1 1) 'E1U)
                                       (make-cor 1 2) 'E1L)])
        (check-equal? (gamestate-layout result) expected-layout)
        (check-equal? (gamestate-player1 result)
                      (make-player1 (make-cor 1 1) "U"))
        (check-equal? (gamestate-player2 result)
                      (make-player2 (make-cor 1 2) "L"))))
    (test-case "bombs with chain explosion, no players affected"
      (let* (
             [layout-all-W
              (vector
               (vector 'W 'W 'W)
               (vector 'W 'W 'W)
               (vector 'W 'W 'W))]
             [bomb-list (list (make-bombstate (make-cor 1 1) 0 'P1)
                              (make-bombstate (make-cor 0 1) 3 'P2))]
             [player1 (make-player1 (make-cor 2 2) "U")]
             [player2 (make-player2 (make-cor 0 0) "D")]
             [gamestate (make-gamestate layout-all-W bomb-list player1 player2 120 6 #f)]
             [result (boom gamestate)]
             [expected-layout (convert (convert layout-all-W
                                                (list (make-cor 1 1)
                                                      (make-cor 0 1)
                                                      (make-cor 2 1)
                                                      (make-cor 1 0)
                                                      (make-cor 1 2)) 'E1)
                                       (make-cor 0 1) 'E1)])
        (check-equal? (gamestate-layout result) expected-layout)
        (check-equal? (gamestate-player1 result) player1)
        (check-equal? (gamestate-player2 result) player2)))
  (test-case "single bomb with player2 in range"
    (let* ([layout-all-W
              (vector
               (vector 'W 'W 'W)
               (vector 'W 'W 'W)
               (vector 'W 'W 'W))]
           [bomb-list (list (make-bombstate (make-cor 1 1) 0 'P1))]
           [player1 (make-player1 (make-cor 0 0) "U")]
           [player2 (make-player2 (make-cor 1 1) "D")]
           [gamestate (make-gamestate layout-all-W bomb-list player1 player2 120 6 #f)]
           [result (boom gamestate)]
           [expected-layout (convert (convert layout-all-W
                                              (list (make-cor 0 1) 
                                                    (make-cor 2 1) 
                                                    (make-cor 1 0) 
                                                    (make-cor 1 2)) 'E1)
                                     (make-cor 1 1) 'E1D)])
      (check-equal? (gamestate-layout result) expected-layout)
      (check-equal? (gamestate-player1 result) player1)
      (check-equal? (gamestate-player2 result)
                    (make-player2 (make-cor 1 1) "D"))))
  (test-case "chain explosion with player1 in range"
    (let* ([layout-all-W
              (vector
               (vector 'W 'W 'W)
               (vector 'W 'W 'W)
               (vector 'W 'W 'W))]
           [bomb-list (list (make-bombstate (make-cor 1 1) 0 'P1)
                            (make-bombstate (make-cor 0 1) 3 'P2))]
           [player1 (make-player1 (make-cor 1 1) "L")]
           [player2 (make-player2 (make-cor 2 2) "U")]
           [gamestate (make-gamestate layout-all-W bomb-list player1 player2 120 6 #f)]
           [result (boom gamestate)]
           [expected-layout (convert (convert (convert layout-all-W
                                                      (list (make-cor 0 1)
                                                            (make-cor 2 1)
                                                            (make-cor 1 0)
                                                            (make-cor 1 2)) 'E1)
                                              (make-cor 1 1) 'E1L)
                                     (make-cor 0 1) 'E1)])
      (check-equal? (gamestate-layout result) expected-layout)
      (check-equal? (gamestate-player1 result)
                    (make-player1 (make-cor 1 1) "L"))
      (check-equal? (gamestate-player2 result) player2)))
  (test-case "no chain explosion"
    (let* ([layout-all-W
              (vector
               (vector 'W 'W 'W)
               (vector 'W 'W 'W)
               (vector 'W 'W 'W))]
           [bomb-list (list (make-bombstate (make-cor 1 1) 0 'P1)
                            (make-bombstate (make-cor 0 0) 3 'P2))]
           [player1 (make-player1 (make-cor 2 2) "U")]
           [player2 (make-player2 (make-cor 0 2) "L")]
           [gamestate (make-gamestate layout-all-W bomb-list player1 player2 120 6 #f)]
           [result (boom gamestate)]
           [expected-layout (convert layout-all-W
                                      (list (make-cor 1 1) 
                                            (make-cor 0 1) 
                                            (make-cor 2 1) 
                                            (make-cor 1 0) 
                                            (make-cor 1 2)) 'E1)])
      (check-equal? (gamestate-layout result) expected-layout)
      (check-equal? (gamestate-player1 result) player1)
      (check-equal? (gamestate-player2 result) player2)))
  (test-case "bombs with countdown > 0 do not explode"
    (let* ([layout-all-W
              (vector
               (vector 'W 'W 'W)
               (vector 'W 'W 'W)
               (vector 'W 'W 'W))]
           [bomb-list (list (make-bombstate (make-cor 1 1) 3 'P1)
                            (make-bombstate (make-cor 2 2) 2 'P2))]
           [player1 (make-player1 (make-cor 0 0) "U")]
           [player2 (make-player2 (make-cor 2 2) "D")]
           [gamestate (make-gamestate layout-all-W bomb-list player1 player2 120 6 #f)]
           [result (boom gamestate)])
      (check-equal? (gamestate-layout result) layout-all-W)
      (check-equal? (gamestate-player1 result) player1)
      (check-equal? (gamestate-player2 result) player2)))
  (test-case "chain explosion and both players in range"
    (let* ([layout-all-W
              (vector
               (vector 'W 'W 'W)
               (vector 'W 'W 'W)
               (vector 'W 'W 'W))]
           [bomb-list (list (make-bombstate (make-cor 1 1) 0 'P1)
                            (make-bombstate (make-cor 0 1) 3 'P2))]
           [player1 (make-player1 (make-cor 0 1) "U")]
           [player2 (make-player2 (make-cor 1 2) "R")]
           [gamestate (make-gamestate layout-all-W bomb-list player1 player2 120 6 #f)]
           [result (boom gamestate)]
           [expected-layout (convert (convert (convert (convert layout-all-W
                                                               (list (make-cor 1 1) 
                                                                     (make-cor 2 1) 
                                                                     (make-cor 1 0)) 'E1)
                                                      (make-cor 1 1) 'E1U)
                                              (make-cor 0 1) 'E1)
                                     (make-cor 1 2) 'E1R)])
      (check-equal? (gamestate-layout result) expected-layout)
      (check-equal? (gamestate-player1 result)
                    (make-player1 (make-cor 0 1) "U"))
      (check-equal? (gamestate-player2 result)
                    (make-player2 (make-cor 1 2) "R"))))
  ))
  
  


;;homepage
(define homepage-tests
  (test-suite "homepage state tests"
    (test-case "timehandler does not change homepage state"
      (check-equal? (timehandler homepage-state) homepage-state))))


;;in-game
(define in-game-tests
  (let (
        [test-layout
         (vector
          (vector 'W 'W 'W)
          (vector 'W 'W 'W)
          (vector 'W 'W 'W))]
        )
  (test-suite "timehandler tests"
    (test-case "empty layout with no bombs"
      (let (
            [example-state
             (make-gamestate test-layout
                             '()
                             (make-player1 (make-cor 0 0) "U")
                             (make-player2 (make-cor 2 2) "D")
                             10
                             3
                             #f)]
            )
        (check-equal? (timehandler example-state)
                      (make-gamestate test-layout
                                      '()
                                      (make-player1 (make-cor 0 0) "U") 
                                      (make-player2 (make-cor 2 2) "D")
                                      9
                                      3
                                      #f))))
    (test-case "bomb countdown decreases"
      (let (
            [example-state
             (make-gamestate test-layout
                             (list (make-bombstate (make-cor 1 1) 3 'P1)
                                   (make-bombstate (make-cor 2 2) 2 'P2))
                             (make-player1 (make-cor 0 0) "U")
                             (make-player2 (make-cor 2 2) "D")
                             10
                             3
                             #f)]
            )
        (check-equal? (timehandler example-state)
                      (make-gamestate test-layout
                                      (list (make-bombstate (make-cor 1 1) 2 'P1)
                                            (make-bombstate (make-cor 2 2) 1 'P2))
                                      (make-player1 (make-cor 0 0) "U") 
                                      (make-player2 (make-cor 2 2) "D")
                                      9
                                      3
                                      #f))))
    (test-case "roundtimer triggers maximum increase"
      (let (
            [example-state
             (make-gamestate test-layout
                             '()
                             (make-player1 (make-cor 0 0) "U")
                             (make-player2 (make-cor 2 2) "D")
                             90
                             3
                             #f)]
            )
        (check-equal? (timehandler example-state)
                      (make-gamestate test-layout
                                      '()
                                      (make-player1 (make-cor 0 0) "U") 
                                      (make-player2 (make-cor 2 2) "D")
                                      89
                                      4
                                      #f))))
    (test-case "maximum does not exceed limit"
      (let (
            [example-state
             (make-gamestate test-layout
                             '()
                             (make-player1 (make-cor 0 0) "U")
                             (make-player2 (make-cor 2 2) "D")
                             30
                             6
                             #f)]
            )
        (check-equal? (timehandler example-state)
                      (make-gamestate test-layout
                                      '()
                                      (make-player1 (make-cor 0 0) "U") 
                                      (make-player2 (make-cor 2 2) "D")
                                      29
                                      6
                                      #f))))



    )))



;;check-died?-tests
(define check-died?-tests
  (test-suite "stop-when tests"
    (test-case "player 1 died"
      (check-true (check-player1-died? 'E1L))
      (check-true (check-player1-died? 'E1R))
      (check-true (check-player1-died? 'E1U))
      (check-true (check-player1-died? 'E1D))
      (check-false (check-player1-died? 'E2L))
      (check-false (check-player1-died? 'W))
      (check-false (check-player1-died? 'D)))

    (test-case "player 2 died"
      (check-true (check-player2-died? 'E2L))
      (check-true (check-player2-died? 'E2R))
      (check-true (check-player2-died? 'E2U))
      (check-true (check-player2-died? 'E2D))
      (check-false (check-player2-died? 'E1L))
      (check-false (check-player2-died? 'W))
      (check-false (check-player2-died? 'D)))

    (test-case "both players died"
      (check-true (check-all-died? 'E1L 'E2R))
      (check-true (check-all-died? 'E1R 'E2L))
      (check-true (check-all-died? 'E1U 'E2D))
      (check-true (check-all-died? 'E1D 'E2U))
      (check-false (check-all-died? 'E1L 'W))
      (check-false (check-all-died? 'W 'E2R))
      (check-false (check-all-died? 'W 'W)))))


;; end?-tests
(define end?-tests
  (test-suite "end? Tests"
    (test-case "game quit"
      (let* ([example-layout (vector
                              (vector 'W 'W 'W)
                              (vector 'W 'W 'W)
                              (vector 'W 'W 'W))]
            [example-state
             (make-gamestate example-layout
                             '()
                             (make-player1 (make-cor 0 0) "U")
                             (make-player2 (make-cor 2 2) "D")
                             10
                             3
                             #t)])
        (check-true (end? example-state))))
    (test-case "homepage state"
      (let* ([example-layout (vector
                              (vector 'W 'W 'W)
                              (vector 'W 'W 'W)
                              (vector 'W 'W 'W))]
            [example-state
             (make-gamestate homepage
                             '()
                             (make-player1 (make-cor 0 0) "U")
                             (make-player2 (make-cor 2 2) "D")
                             10
                             3
                             #f)])
        (check-false (end? example-state))))
    (test-case "both players died"
      (let* ([example-layout (vector
                              (vector 'W 'W 'W)
                              (vector 'W 'W 'W)
                              (vector 'W 'W 'W))]
            [example-state
             (make-gamestate (convert example-layout (list (make-cor 0 0) (make-cor 2 2)) 'E1L)
                             '()
                             (make-player1 (make-cor 0 0) "U")
                             (make-player2 (make-cor 2 2) "D")
                             10
                             3
                             #f)])
        (check-true (end? example-state))))
    (test-case "player 1 died"
      (let* ([example-layout (vector
                              (vector 'W 'W 'W)
                              (vector 'W 'W 'W)
                              (vector 'W 'W 'W))]
            [example-state
             (make-gamestate (convert example-layout (list (make-cor 0 0)) 'E1U)
                             '()
                             (make-player1 (make-cor 0 0) "U")
                             (make-player2 (make-cor 2 2) "D")
                             10
                             3
                             #f)])
        (check-true (end? example-state))))

    ;;
    (test-case "Player 2 died"
      (let* ([example-layout (vector
                              (vector 'W 'W 'W)
                              (vector 'W 'W 'W)
                              (vector 'W 'W 'W))]
            [example-state
             (make-gamestate (convert example-layout (list (make-cor 2 2)) 'E2D)
                             '()
                             (make-player1 (make-cor 0 0) "U")
                             (make-player2 (make-cor 2 2) "D")
                             10
                             3
                             #f)])
        (check-true (end? example-state))))
    (test-case "time finishes"
      (let* ([example-layout (vector
                              (vector 'W 'W 'W)
                              (vector 'W 'W 'W)
                              (vector 'W 'W 'W))]
            [example-state
             (make-gamestate example-layout
                             '()
                             (make-player1 (make-cor 0 0) "U")
                             (make-player2 (make-cor 2 2) "D")
                             0
                             3
                             #f)])
        (check-true (end? example-state))))
    (test-case "game continues"
      (let* ([example-layout (vector
                              (vector 'W 'W 'W)
                              (vector 'W 'W 'W)
                              (vector 'W 'W 'W))]
            [example-state
             (make-gamestate example-layout
                             '()
                             (make-player1 (make-cor 0 0) "U")
                             (make-player2 (make-cor 2 2) "D")
                             10
                             3
                             #f)])
        (check-false (end? example-state))))))


(define generate-random-layout-tests
  (test-suite
   "testing random-layout"
   (let ([layout (generate-layout 0 '() random-layout-rule)])
     (test-case "check fixed player1"
                (check-equal? (get-symbol layout (cor 0 0)) 'W1D))
     (test-case "check fixed player2"
                (check-equal? (get-symbol layout (cor (- MAX-COLS 1) (- MAX-ROWS 1))) 'W2U))
     (test-case "check fixed 'W"
                (begin
                  (check-equal? (get-symbol layout (cor 0 1)) 'W)
                  (check-equal? (get-symbol layout (cor 1 0)) 'W)
                  (check-equal? (get-symbol layout (cor 1 1)) 'W)
                  (check-equal? (get-symbol layout (cor (- MAX-COLS 2) (- MAX-ROWS 2))) 'W)
                  (check-equal? (get-symbol layout (cor (- MAX-COLS 2) (- MAX-ROWS 1))) 'W)
                  (check-equal? (get-symbol layout (cor (- MAX-COLS 1) (- MAX-ROWS 2))) 'W)))
     (test-case "check fixed 'I"
                (for ([row (in-range MAX-ROWS)]
                      [col (in-range MAX-COLS)])
                  (when (is-I? row col)
                    (check-equal? (vector-ref (vector-ref layout row) col) 'I))))
     (test-case "check remaining cells are 'W or 'D"
           (for ([row (in-range MAX-ROWS)]
                 [col (in-range MAX-COLS)])
             (unless (or (is-I? row col)
                         (is-player1? row col)
                         (is-player2? row col)
                         (is-fixed-W? row col))
               (let ([value (vector-ref (vector-ref layout row) col)])
                 (check-true
                  (or (equal? value 'W)
                      (equal? value 'D))))))))))


(define generate-homepage-layout-tests
  (test-suite
   "testing homepage"
   (let ([layout homepage])
     (test-case "check 'D in YU shape"
                (for ([row (in-range MAX-ROWS)]
                      [col (in-range MAX-COLS)])
                  (when (is-YU? row col)
                    (check-equal? (vector-ref (vector-ref layout row) col) 'D))))
     (test-case "check 'B in HU shape"
                (for ([row (in-range MAX-ROWS)]
                      [col (in-range MAX-COLS)])
                  (when (is-HU? row col)
                    (check-equal? (vector-ref (vector-ref layout row) col) 'B))))
     (test-case "check remaining cells are 'W"
                (for ([row (in-range MAX-ROWS)]
                      [col (in-range MAX-COLS)])
                  (unless (or (is-YU? row col)
                              (is-HU? row col))
                    (check-equal? (vector-ref (vector-ref layout row) col) 'W)))))))


(define test-all
  (test-suite "All Tests"
    remove-bomb-tests
    check-zero-tests
    updated-bomb-tests
    updated-roundtimer-tests
    check-roundtimer?-tests
    add-maximum-tests
    updated-E-symbol-tests
    updated-layout-tests
    extend-direction-tests
    single-boom-range-tests
    boom-range-tests
    chain-explosion-tests
    cor=?-tests
    in-boom-cor?-tests
    get-symbol-tests
    check-I?-tests
    check-D?-tests
    convert-tests
    homepage-tests
    boom-tests
    in-game-tests
    check-died?-tests
    end?-tests
    generate-random-layout-tests
    generate-homepage-layout-tests))



(run-tests test-all)


