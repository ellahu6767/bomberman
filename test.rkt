;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;define the function vector-map
;function is same to map but can used in vector
;(vector-map f vec) = (map f vec)
;(vector-map-2d f vec) = (map f (vec(vec)))
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

;;vector position any -> vector
(define (convert vec cor x)
  (begin
  (vector-set!
   (vector-ref vec
               (cor-row cor))
   (cor-column cor)
   x)
  vec))

;;coordinate is a structure
(define-struct cor [column row])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct gamestate [map bomb player1 player2])
(define-struct player1 [posn dead?])
(define-struct player2 [posn dead?])

;bomb is a list of structure
(define-struct bomb! [cor countdown])
;represents the countdown and coordinate of the bomb
;date exampels
(list
 (make-cor 1 2) 4
 (make-cor 2 5) 3)




;constant definitions
(define CELL-SIZE 30)
(define player-image (circle 15 "solid" "red"))
(define velocity 20)

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



;vector(vector -> vector(vector
; radom # and .
(define (generate-map axiom)
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

(define map1(generate-map axiom))



;render map

;render cell + space
;(overlay space(cell))
(define (render-cell char)
  (let ((fill-color
         (cond
           [(char=? char #\#) "green"]
           [(char=? char #\.) "white"]
           [(char=? char #\*) "black"]
           [(char=? char #\S) "blue"]
           [(char=? char #\B) "black"]
           [(char=? char #\T) "red"])))
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
;map1 is a vector(vector
;map1 -> Image
(define (render-map map1)
  (let loop ((i 0) (n (vector-length map1)) (acc empty-image))
    (if (>= i n)
        acc
        (loop (+ i 1) n
              (above acc
                     (combine-images-left-right (vector-ref map1 i)))))))

;render
;gamestate -> Image
(define (render gamestate)
  (place-image
   player-image
   (posn-x (player1-posn (gamestate-player1 gamestate)))
   (posn-y (player1-posn (gamestate-player1 gamestate)))
   (render-map map1)))

                 




;keyhandler
;gamestate ke -> gamestate
;up down left right only change the position of player

;auxiliary functions

;Posn -> Coordinate
(define (nearest-cor posn)
  (make-cor
  (round (/ (posn-x posn) CELL-SIZE))
  (round (/ (posn-y posn) CELL-SIZE))))
  

;gamestate -> gamestate
(define (put-bomb gamestate)
  (let ([bomb-cor (nearest-cor
                   (player1-posn
                    (gamestate-player1 gamestate)))])
    (make-gamestate
     (convert (gamestate-map gamestate) bomb-cor #\B) 
     (cons (make-bomb! bomb-cor 4)                  
           (gamestate-bomb gamestate))
     (gamestate-player1 gamestate)
     (gamestate-player2 gamestate))))

   

   
   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;gamestate -> gamestate
(define (move-down gamestate)
  (make-gamestate
   (gamestate-map gamestate)
   (gamestate-bomb gamestate)
   (make-player1 (make-posn
                  (posn-x
                   (player1-posn
                    (gamestate-player1 gamestate)))
                  (+ (posn-y
                      (player1-posn
                       (gamestate-player1 gamestate)))
                     velocity))
                 #f)
   (gamestate-player2 gamestate)))

(define (move-up gamestate)
  (make-gamestate
   (gamestate-map gamestate)
   (gamestate-bomb gamestate)
   (make-player1 (make-posn
                  (posn-x
                   (player1-posn
                    (gamestate-player1 gamestate)))
                  (- (posn-y
                      (player1-posn
                       (gamestate-player1 gamestate)))
                     velocity))
                 #f)
   (gamestate-player2 gamestate)))

(define (move-left gamestate)
  (make-gamestate
   (gamestate-map gamestate)
   (gamestate-bomb gamestate)
   (make-player1 (make-posn
                  (-
                   (posn-x
                   (player1-posn
                    (gamestate-player1 gamestate)))
                   velocity)
                  (posn-y
                      (player1-posn
                       (gamestate-player1 gamestate))))
                 #f)
   (gamestate-player2 gamestate)))

(define (move-right gamestate)
  (make-gamestate
   (gamestate-map gamestate)
   (gamestate-bomb gamestate)
   (make-player1 (make-posn
                  (+
                   (posn-x
                   (player1-posn
                    (gamestate-player1 gamestate)))
                   velocity)
                  (posn-y
                      (player1-posn
                       (gamestate-player1 gamestate))))
                 #f)
   (gamestate-player2 gamestate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;gamestate ke -> gamestate
(define (move gamestate ke)
  (cond
    [(string=? "down" ke)
     ;;(check-barrier?)
     (move-down gamestate)]

    [(string=? "up" ke)
     ;;(check-barrier?)
     (move-up gamestate)]

    [(string=? "right" ke)
     ;;(check-barrier?)
     (move-right gamestate)]

    [(string=? "left" ke)
     ;;(check-barrier?)
     (move-left gamestate)]

    

    ;;.... up left right

    ;;bomb
    [(string=? "a" ke)
     ;;(check-bomb?)
     (put-bomb gamestate)]
                
    [else gamestate]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;on-tick

;auxilirary function

;list<bomb> -> list<bomb>
(define (new-timer bomb)
  (map (lambda (bomb!)
         (make-bomb!
          (bomb!-cor bomb!)
          (sub1 (bomb!-countdown bomb!))))
       bomb))

           
;;;;;;;question
;;;;;;;how to use map of the one field of the list of structure elegantly
 

;auxiliary functions
;list<bomb> -> Boolean
(define (check-two? lob)
  (cond
    [(empty? lob) #f]
    [else
     (if
     (<= (bomb!-countdown (first lob)) 2)
     #t
     (check-two? (rest lob)))]))

;;constant defintions
(define boom-distance 3)



;boom-cor:
;list<bomb> -> list<cor>
;examples
;bomb boom in (5,5)
;(5+- 5, 5+-5) will be convert to #\.)

;;这个function不像人写的，后面想想怎么优化
;;11.9 发现input错了，但是没改写法，后面一定想想怎么改
; list<bomb!> -> list<cor>
(define (boom-cor bomb-list)
  (if (empty? bomb-list)
      '()
      (append
       (list
        (make-cor (cor-column (bomb!-cor (first bomb-list)))
                  (cor-row (bomb!-cor (first bomb-list)))))
       (list 
         (make-cor (- (cor-column (bomb!-cor (first bomb-list))) 1) 
                  (cor-row (bomb!-cor (first bomb-list))))
         (make-cor (- (cor-column (bomb!-cor (first bomb-list))) 2) 
                  (cor-row (bomb!-cor (first bomb-list))))
         (make-cor (- (cor-column (bomb!-cor (first bomb-list))) 3) 
                  (cor-row (bomb!-cor (first bomb-list)))))
       (list 
         (make-cor (+ (cor-column (bomb!-cor (first bomb-list))) 1) 
                  (cor-row (bomb!-cor (first bomb-list))))
         (make-cor (+ (cor-column (bomb!-cor (first bomb-list))) 2) 
                  (cor-row (bomb!-cor (first bomb-list))))
         (make-cor (+ (cor-column (bomb!-cor (first bomb-list))) 3) 
                  (cor-row (bomb!-cor (first bomb-list)))))
       (list 
         (make-cor (cor-column (bomb!-cor (first bomb-list))) 
                  (- (cor-row (bomb!-cor (first bomb-list))) 1))
         (make-cor (cor-column (bomb!-cor (first bomb-list))) 
                  (- (cor-row (bomb!-cor (first bomb-list))) 2))
         (make-cor (cor-column (bomb!-cor (first bomb-list))) 
                  (- (cor-row (bomb!-cor (first bomb-list))) 3)))
       (list 
         (make-cor (cor-column (bomb!-cor (first bomb-list))) 
                  (+ (cor-row (bomb!-cor (first bomb-list))) 1))
         (make-cor (cor-column (bomb!-cor (first bomb-list))) 
                  (+ (cor-row (bomb!-cor (first bomb-list))) 2))
         (make-cor (cor-column (bomb!-cor (first bomb-list))) 
                  (+ (cor-row (bomb!-cor (first bomb-list))) 3)))
       (boom-cor (rest bomb-list)))))



  
;;这里想想能不能优化一下
;; apply-convert-to-list:
;; vector (list of cor) char -> vector
(define (apply-convert-to-list map cor-list char)
  (if (empty? cor-list)
      map  
      (apply-convert-to-list 
       (convert map (first cor-list) char)  
       (rest cor-list)                   
       char)))



;gamestate list<cor> -> gamestate
; gamestate -> gamestate
(define (boom! gamestate)
  (make-gamestate
   (apply-convert-to-list
    (gamestate-map gamestate)
    (boom-cor (gamestate-bomb gamestate))  
    #\T)  ; 
   (new-timer (gamestate-bomb gamestate))
   (gamestate-player1 gamestate)
   (gamestate-player2 gamestate)))

    
   ;;remove bomb from the list
  ;; (gamestate-bomb gamestate)
  ;; (gamestate-player1 gamestate)
  ;; (gamestate-player2 gamestate)))


;;gamestate -> Boolean
;list<bomb> -> Boolean
(define (check-zero? lob)
  (cond
    [(empty? lob) #f]
    [else
     (if
     (<= (bomb!-countdown (first lob)) 0)
     #t
     (check-zero? (rest lob)))]))

;;gamestate -> gamestate
(define (bomb-end gamestate)
  (make-gamestate
   (apply-convert-to-list
    (gamestate-map gamestate)
    (boom-cor (gamestate-bomb gamestate))  
    #\.)
    (remove-bomb! (gamestate-bomb gamestate))
    (gamestate-player1 gamestate)
    (gamestate-player2 gamestate)))

;;list<bomb!> -> list<bomb!>
(define (remove-bomb! lob)
  (cond
    [(empty? lob) '()]
    [else
     (if
      (= (bomb!-countdown (first lob)) 0)
      (rest lob)
      (remove-bomb! (rest lob)))]))
    
              
;gamestate -> gamestate
(define (time-control gamestate)
  (let ((updated-gamestate
         (make-gamestate (gamestate-map gamestate)
                         (new-timer (gamestate-bomb gamestate))
                         (gamestate-player1 gamestate)
                         (gamestate-player2 gamestate))))
    (cond
      [(check-zero? (gamestate-bomb updated-gamestate))
       (bomb-end updated-gamestate)]
      [(check-two? (gamestate-bomb updated-gamestate))
       (boom! updated-gamestate)]
      [else
       updated-gamestate])))

   

   
              
;main function
(define (main gamestate)
  (big-bang gamestate
    [to-draw render]
    [on-key move]
    [on-tick time-control 1]))
              
(define initial-state
  (make-gamestate
   map1
   '()
   (make-player1 (make-posn 10 10) #f)
   (make-player2 (make-posn 100 100) #f)))


;application
(main initial-state)






;;;;;;;;;;;;;;;;
;;开发日志
;;11.8

;;1.
;;有大量边界检测没有处理

;;2.
;;思考了一下炸弹的逻辑
;;由on-tick函数处理以下逻辑：

;;炸弹爆炸倒计时以内#\B
;;炸弹倒计时结束，对boom-range（包含自己） #\T (特效渲染）
;;特效渲染也有自己的倒计时，我们应该不能做成渐变
;;特效渲染倒计时结束后，#\.（空地）

;;3.
;;难点在于，如何确保#\#不被炸毁？

;;4.
;;convert如何处理一个list<cor>


;;11.9

;;5.

;;mark: we should try to merge check-zero of each boom! and 爆炸特效;;
;; think about use negetive number here ,zero means boom, -3 means 爆炸特效消失
;; notice that the textbook disencourage us to use negetive number
;; [(check-negetive3? (gamestate

;;对于以上问题
;;想到可以从6开始计数
;;2=炸弹爆炸,#\T
;;0=特效结束
;;这样避免了使用负数

;;6.
;;time control 使用cond的逻辑问题
;;cond会直接返回第一个满足条件的condition
;;想到可以先check-zero？
;;then check-two?
;;but it still not work after the first bomb
;;发现问题在于没有移除炸弹，导致第一个炸弹归0之后auxiliary
;;function always return #t
;;so condition (check-zero?) always #t
;;添加移除炸弹的function解决问题？

;;7.
;;发现同时放置多个炸弹只处理最后一个
;;意料之中，毕竟list<cor>function写的太烂了
;;发现是因为input/output错了，应该是list->list


;;8.
;;发现round（posn）返回的cor非常不准确
;;留着以后改
;;当然，这个问题实际上非常重要
;;他同时涉及到dead？的检测，属于游戏核心玩法
;;至少要实现视觉效果和实际效果接近


;;9
;;主要功能还差：
;;1. 边界检测
;;2. 对于不可破坏的墙的检测



















