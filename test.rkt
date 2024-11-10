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
(define player-image (circle 10 "solid" "red"))
(define velocity 20)

; axiom:
; axiom is a (vector (vector..))

; represents the 11 * 15 map of the game
; use string to represent the type of the elements in the map

; S = safe area
; # = green area (can be dismentaled)
; * = black area (can not be dismentaled)
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
   (floor (/ (posn-x posn) CELL-SIZE))
   (floor (/ (posn-y posn) CELL-SIZE))))

  

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

;; check-border: gamestate cor -> Boolean

(define (check-border gamestate cor)
  (and (<= 0 (cor-column cor) 14)   
       (<= 0 (cor-row cor) 10)      
       (let ((element (get-element (gamestate-map gamestate) cor)))
         (not (or (char=? element #\#)
                  (char=? element #\*))))))


;; move: gamestate key -> gamestate
(define (move gamestate ke)
  (cond
    [(string=? ke "down")
     (let* ((player (gamestate-player1 gamestate))
            (new-pos (make-posn (posn-x (player1-posn player))
                                 (+ (posn-y (player1-posn player)) velocity)))
            (new-cor (nearest-cor new-pos)))
       (if (check-border gamestate new-cor)
           (move-down gamestate)
           gamestate))]
    [(string=? ke "up")
     (let* ((player (gamestate-player1 gamestate))
            (new-pos (make-posn (posn-x (player1-posn player))
                                 (- (posn-y (player1-posn player)) velocity)))
            (new-cor (nearest-cor new-pos)))
       (if (check-border gamestate new-cor)
           (move-up gamestate)
           gamestate))]
    [(string=? ke "left")
     (let* ((player (gamestate-player1 gamestate))
            (new-pos (make-posn (- (posn-x (player1-posn player)) velocity)
                                 (posn-y (player1-posn player))))
            (new-cor (nearest-cor new-pos)))
       (if (check-border gamestate new-cor)
           (move-left gamestate)
           gamestate))]
    [(string=? ke "right")
     (let* ((player (gamestate-player1 gamestate))
            (new-pos (make-posn (+ (posn-x (player1-posn player)) velocity)
                                 (posn-y (player1-posn player))))
            (new-cor (nearest-cor new-pos)))
       (if (check-border gamestate new-cor)
           (move-right gamestate)
           gamestate))]
    [(string=? ke "a")
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
;;11.10加入条件检查，修改部分代码


;cor -> char
;get the char in a certain cor
(define (get-element vec cor)
  (vector-ref
   (vector-ref vec (cor-row cor))
   (cor-column cor)))



; list<bomb!> vec -> list<cor>
(define (boom-cor bomb-list vec)
  (if (empty? bomb-list)
      '()
      (let* ((center (bomb!-cor (first bomb-list)))
             (cx (cor-column center))
             (cy (cor-row center))
             (potential-coords 
               (append
                (list (make-cor cx cy))
                (list (make-cor (- cx 1) cy) (make-cor (- cx 2) cy) (make-cor (- cx 3) cy))
                (list (make-cor (+ cx 1) cy) (make-cor (+ cx 2) cy) (make-cor (+ cx 3) cy))
                (list (make-cor cx (- cy 1)) (make-cor cx (- cy 2)) (make-cor cx (- cy 3)))
                (list (make-cor cx (+ cy 1)) (make-cor cx (+ cy 2)) (make-cor cx (+ cy 3))))))
        (append
         (filter (lambda (coord)
                   (and
                    (<= 0 (cor-column coord) 14)
                    (<= 0 (cor-row coord) 10)
                    (not (char=? #\* (get-element vec coord)))))
                 potential-coords)
         (boom-cor (rest bomb-list) vec)))))




  
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
    (boom-cor (gamestate-bomb gamestate)
              (gamestate-map gamestate))  
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
    (boom-cor (gamestate-bomb gamestate)
              (gamestate-map gamestate))  
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

;;9.
;;发现最早的炸弹爆炸会导致所有的炸弹爆炸
;;boom！ 没有正确递归
;;正确逻辑：
;;炸弹可以链式爆炸，这符合大多数炸弹人游戏规范
;;但目前的情况是炸弹炸不到炸弹也会触发
;;if 炸弹们的（bomb！-cor list）在爆炸的炸弹的boom-cor list中
;;将炸弹们的（bomb！-countdown）设置为2
;;否则，不处理


;;10.
;;主要功能还差：
;;1. 边界检测
;;2. 对于不可破坏的墙的检测

;;11.10
;;加入了炸弹爆炸的边际检测
;;1.渲染的逻辑应该是：一个方向只要遇到墙就不渲染
;;但目前的情况是：只有遇到墙的cell不渲染

;;2.主要功能完全实现
;;剩下：
;;今天发现的问题1.
;;炸弹的链式反应处理
;;新的data definitions 要更改一些，但都是小功能，很好实现


;;难度上看
;;on-tick最难(造了convert 和 get-element）,大量recursion，耗时最长
;;接着是render 地图部分(需要自己造vector-map，学生语言没有这个）
;;on-key 因为已经造了 get-element，所以难度适中
;;stop-when = render 开始菜单 = render countdown栏 ，这三个比较容易

;;难以定义的难度：
;;绘图/图像处理

























