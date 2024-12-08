;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/base)
(require "bomberman.rkt")
(require rackunit)
(require rackunit/text-ui)

;; example of bomblist
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

;;example layout and bomb cor
;;for each kind of example layout,do twice test
;;first one in the center
;;second one in the different corner
(define center-cor (make-cor 1 1))

(define layout-all-W
  (vector
   (vector 'W 'W 'W)
   (vector 'W 'W 'W)
   (vector 'W 'W 'W)))

(define southwest-corner-cor (make-cor 0 2))

(define layout-in-many-D
  (vector
   (vector 'W 'D 'D)
   (vector 'D 'W 'D)
   (vector 'D 'D 'D)))

(define northwest-corner-cor (make-cor 0 0))

(define layout-in-many-I
  (vector
   (vector 'I 'I 'I)
   (vector 'I 'W 'I)
   (vector 'I 'I 'W)))

(define southeast-corner-cor (make-cor 2 2))

(define layout-in-mix-D-I
  (vector
   (vector 'I 'D 'W)
   (vector 'D 'W 'I)
   (vector 'I 'I 'I)))

(define northeast-corner-cor (make-cor 2 0))

   

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
                                (make-bombstate (make-cor 4 4) 0 'P2))))))

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


;updated-layout-test
(test-begin "update E symbol test"
  (check-equal? (update-E-symbols 'E2) 'E1)
  (check-equal? (update-E-symbols 'E1) 'E0)
  (check-equal? (update-E-symbols 'E0) 'W)
  (check-equal? (update-E-symbols 'W) 'W))


;extend-direction
(define extend-direction-tests
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
                               '())));;;;modify bound to make it can test more bound issue
  ))


;;single-boom-range-tests
(define single-boom-range-tests
  (test-suite "single boom range tests"
    ;; layout-all-W
    (test-case "all W layout: center bomb only"
      (let ([bomb (make-bombstate center-cor 0 'P1)])
        (check-equal? (single-boom-range bomb layout-all-W)
                      (list (make-cor 1 1)
                            (make-cor 1 0)
                            (make-cor 1 2)
                            (make-cor 0 1)
                            (make-cor 2 1))))) ; center bomb

    ;; layout-in-many-D
    (test-case "many D layout: center bomb only"
      (let ([bomb (make-bombstate center-cor 0 'P1)])
        (check-equal? (single-boom-range bomb layout-in-many-D)
                      (list (make-cor 1 1)
                            (make-cor 1 0)
                            (make-cor 1 2) 
                            (make-cor 0 1)
                            (make-cor 2 1))))) ; center bomb

    ;; layout-in-many-I
    (test-case "many I layout: center bomb only"
      (let ([bomb (make-bombstate center-cor 0 'P1)])
        (check-equal? (single-boom-range bomb layout-in-many-I)
                      (list (make-cor 1 1))))) ; center bomb only, blocked by 'I

    ;; layout-in-mix-D-I
    (test-case "mix D and I layout: center bomb only"
      (let ([bomb (make-bombstate center-cor 0 'P1)])
        (check-equal? (single-boom-range bomb layout-in-mix-D-I)
                      (list (make-cor 1 1)
                            (make-cor 1 0) 
                            (make-cor 0 1))))) ; center bomb, blocked by 'I
  ))


;;boom-range-tests
(define boom-range-tests
  (test-suite "boom Range tests"
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
  ))






                   



(define homepage-tests
  (test-suite "homepage state tests"
    (test-case "timehandler does not change homepage-state"
      (check-equal? (timehandler homepage-state) homepage-state))))




