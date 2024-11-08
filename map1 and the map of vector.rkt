;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |map1 and the map of vector|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

;constant definitions
(define CELL-SIZE 20)

; axiom:
; axiom is a (vector (vector..))

; represents the 11 * 15 map of the game
; use string to represent the type of the elements in the map

; S = safe area
; * = green area (can be dismentaled)
; # = black area (can not be dismentaled)
; . =white area (corrider)
(define axiom
  (vector
   (vector #\S #\S #\S #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
   (vector #\S #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
   (vector #\S #\. #\* #\. #\* #\. #\* #\. #\* #\. #\* #\. #\* #\. #\.)
   (vector #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
   (vector #\. #\. #\* #\. #\* #\. #\* #\. #\* #\. #\* #\. #\* #\. #\.)
   (vector #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
   (vector #\. #\. #\* #\. #\* #\. #\* #\. #\* #\. #\* #\. #\* #\. #\.)
   (vector #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
   (vector #\. #\. #\* #\. #\* #\. #\* #\. #\* #\. #\* #\. #\* #\. #\S)
   (vector #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\S)
   (vector #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\. #\S #\S #\S)))

;auxilirary function
;define the function vector-map
;function is same to map but can used in vector
(define (vector-map f vec)
  (let ((len (vector-length vec))) ;get the length
    (let ((result (make-vector len))) ;store the result
      (let resursion ((i 0))  ;define the base i = 0
        (if (>= i len)  ;recursive all the elements in vector
            result      ;output the result
            (begin      ;otherwise 
              (vector-set! result i (f (vector-ref vec i))) ;apply f to the ith element
              (resursion (+ i 1)))))))) 

(define (vector-map-2d f vec)
  (let ((len (vector-length vec)))
    (let ((result (make-vector len)))
      (let resursion ((i 0))
        (if (>= i len)
            result
            (begin
              (vector-set! result i (f (vector-ref vec i)))
              (resursion (+ i 1))))))))

;vector(vector -> vector(vector
; radom # and .
(define (generate-l-system axiom)
  (vector-map-2d   ;map lambda row to all the elements
   (lambda (row)   ;
     (vector-map   ;map lambda ch to all the elements
      (lambda (char) ;
        (cond
          [(char=? char #\#) (if (zero? (random 2)) #\. #\#)]  
          [(char=? char #\.) (if (zero? (random 2)) #\# #\.)]  
          [(char=? char #\*) #\*]                             
          [(char=? char #\S) #\S]))
      row))
   axiom))



;;; test render

;render cell + space
(define (render-cell char)
  (let ((fill-color
         (cond
           [(char=? char #\#) "green"]
           [(char=? char #\.) "white"]
           [(char=? char #\*) "black"]
           [(char=? char #\S) "blue"])))
    (overlay
     (rectangle CELL-SIZE CELL-SIZE "outline" "black")  
     (rectangle CELL-SIZE CELL-SIZE "solid" fill-color))))  

; auxiliary function
(define (combine-images-left-right vec)
  (let loop ((i 0) (n (vector-length vec)) (acc empty-image))
    (if (>= i n)
        acc
        (loop (+ i 1) n (beside acc (render-cell (vector-ref vec i)))))))

; render-map
(define (render-map level)
  (let loop ((i 0) (n (vector-length level)) (acc empty-image))
    (if (>= i n)
        acc
        (loop (+ i 1) n
              (above acc
                     (combine-images-left-right (vector-ref level i)))))))




; application
(render-map (generate-l-system axiom))






















