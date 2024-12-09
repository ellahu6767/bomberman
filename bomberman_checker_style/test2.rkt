;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname test2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/base)
(require "public.rkt")
(require "on_key.rkt")
(require "render.rkt")
(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)

;;----------render----------;;
;;
(define is-I?-tests
  (test-suite " "
   (test-case "row even, col odd, both in bounds"
              (check-true (is-I? 2 3)))
   (test-case "invalid"
              (check-false (is-I? 1 3) "row add")
              (check-false (is-I? 2 0) "out of bound"))))
;;
(define is-player1?-tests
  (test-suite " "
   (test-case "= 0"
              (check-true (is-player1? 0 0)))
   (test-case "not equal to 0 "
              (check-false (is-player1? 0 1)))))
;;
(define is-player2?-tests
  (test-suite " "
   (test-case "not equal to 24"
              (check-false (is-player2? 0 0)))
   (test-case "= 24"
              (check-true (is-player2? 10 14)))))
;;
(define is-fixed-W?-tests
  (test-suite " "
   (test-case "valid"
              (check-true (is-fixed-W? 2 0) "0 < x <= 2")
              (check-true (is-fixed-W? 10 12) ">= 22"))
   (test-case "invalid"
              (check-false (is-fixed-W? 0 0) "= 0")
              (check-false (is-fixed-W? 5 5) "< 0, but also < 2")
              (check-false (is-fixed-W? 10 10) "<= 22 "))))
;;*
(define is-YU?-tests
  (test-suite " "
   (test-case "valid Y"
              (check-true (is-YU? 0 0) "0 <= row=col <=2")
              (check-true (is-YU? 1 3) "0 <= row <= 2, 0 < col <3, col - row = 2, col + row = 4")
              (check-true (is-YU? 3 2) "col = 2, 2<= row <= 4"))
   (test-case "valid U"
              (check-true (is-YU? 0 8) "col = 8, 0 <= row <= 4")
              (check-true (is-YU? 0 12) "col = 12, 0 <= row <= 4")
              (check-true (is-YU? 4 10) "row = 4, 8 <= col <= 12"))
   (test-case "Invalid positions"
              (check-false (is-YU? 5 5) " ")
              (check-false (is-YU? 1 2) "col != row")
              (check-false (is-YU? 9 13) "col > 12"))))
;;
(define is-HU?-tests
  (test-suite " "
   (test-case "valid U"
              (check-true (is-HU? 6 8) "col = 8, 6 <= row <= 10")
              (check-true (is-HU? 10 12) "row = 10, 8 <= col <= 12")
              (check-true (is-HU? 8 12) "col = 12, 6 <= row <= 10"))
   (test-case "valid H"
              (check-true (is-HU? 6 0) "col = 0, 6 <= row <= 10")
              (check-true (is-HU? 6 4) "col = 4, 6 <= row <= 10")
              (check-true (is-HU? 8 4) "row = 8, 0 <= col <= 4"))
   (test-case "invalid"
              (check-false (is-HU? 9 9) "row = 9 != 10")
              (check-false (is-HU? 5 5) "col = 5")
              (check-false (is-HU? 12 10) "row > 10"))))
;;
(define homepage-rule-tests
  (test-suite " "
   (test-case "'D"
              (check-equal? (homepage-rule 0 0) 'D "0 <= row <= 2, 0 <= col <= 2")
              (check-equal? (homepage-rule 1 1) 'D "0 <= row <= 2, 0 <= col <= 2 and col = row")
              (check-equal? (homepage-rule 2 2) 'D "0 <= row <= 2, 0 <= col <= 2 and col = row")
              (check-equal? (homepage-rule 3 2) 'D "0 <= row <= 2, col = 2, and 2 <= row <= 4"))
   (test-case "'B"
              (check-equal? (homepage-rule 6 0) 'B "col = 0, 6 <= row <= 10")
              (check-equal? (homepage-rule 6 4) 'B "col = 4, 6 <= row <= 10")
              (check-equal? (homepage-rule 8 4) 'B "row = 8, 0 <= col <= 4")
              (check-equal? (homepage-rule 6 8) 'B "col = 8, 6 <= row <= 10")
              (check-equal? (homepage-rule 10 12) 'B "row = 10, 8 <= col <= 12"))
   (test-case "'W"
              (check-equal? (homepage-rule 5 8) 'W "not in is-HU? or is-YU")
              (check-equal? (homepage-rule 7 7) 'W "not in is-HU? or is-YU")
              (check-equal? (homepage-rule 11 12) 'W "not in is-HU? or is-YU"))))









