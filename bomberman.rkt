;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname bomberman) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/base)
(require 2htdp/image)
(require 2htdp/universe)
;;搜索*****查看待处理问题

(define (vector-map f vec)
  (let ((len (vector-length vec))) ;get the length
    (let ((result (make-vector len))) ;store the result
      (let resursion ((i 0))  ;define the base i = 0
        (if (>= i len)  ;recursive all the symbols in vector
            result      ;output the result
            (begin      ;otherwise 
              (vector-set! result i (f (vector-ref vec i))) ;apply f to the ith symbol
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

; base:
; base is a Vector<Vector<Symbol>>

; represents the 11 * 15 layout of the game
; use symbol to represent the type of the symbols in the map

; 'S = safe area
; 'U = undestructable area 
; 'W = walkable area
(define base
  (vector
   (vector 'S 'S 'S 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W)
   (vector 'S 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W)
   (vector 'S 'W 'U 'W 'U 'W 'U 'W 'U 'W 'U 'W 'U 'W 'U)
   (vector 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W)
   (vector 'W 'W 'U 'W 'U 'W 'U 'W 'U 'W 'U 'W 'U 'W 'U)
   (vector 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W)
   (vector 'W 'W 'U 'W 'U 'W 'U 'W 'U 'W 'U 'W 'U 'W 'U)
   (vector 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W)
   (vector 'W 'W 'U 'W 'U 'W 'U 'W 'U 'W 'U 'W 'U 'W 'S)
   (vector 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'S)
   (vector 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'W 'S 'S 'S)))


;generate-layout:
;Vector<Vector<Symbol>> -> Vector<Vector<Symbol>>
;convert base to random layout
;random 'W

;implementation
(define (generate-layout base)
  (vector-map-2d  
   (lambda (row)   
     (vector-map  
      (lambda (symbol) 
        (cond
          [(symbol=? symbol 'W) (if (zero? (random 2)) 'D 'W)]
          [else symbol])) 
      row))
   base))

;layout1:
;layout1 is the first kind of layout of the game
(define layout1 (generate-layout base))


(define CELL-SIZE (image-width (bitmap "D.png")))
(define player-image (circle (/ CELL-SIZE 2) "solid" "red"))
(define velocity (/ CELL-SIZE 2))


(define (render-cell symbol)
  (cond
    [(symbol=? symbol 'D) (bitmap "D.png")]
    [(symbol=? symbol 'U) (bitmap "U.png")]
    [(symbol=? symbol 'S) (bitmap "W.png")]
    [(symbol=? symbol 'B) (bitmap "B.png")]
    [(symbol=? symbol 'W) (bitmap "W.png")]
    [(symbol=? symbol 'T) (overlay
     (rectangle CELL-SIZE CELL-SIZE "outline" "black")  
     (rectangle CELL-SIZE CELL-SIZE "solid" "red"))]))
    

; auxiliary function
(define (combine-images-left-right vec)
  (let loop ((i 0) (n (vector-length vec)) (acc empty-image))
    (if (>= i n)
        acc
        (loop (+ i 1) n (beside acc (render-cell (vector-ref vec i)))))))


; render-map
; layout -> Image
(define (render-map layout)
  (let loop ((i 0) (n (vector-length layout)) (acc empty-image))
    (if (>= i n)
        acc
        (loop (+ i 1) n
              (above acc
                     (combine-images-left-right (vector-ref layout i)))))))


;render
;gamestate -> Image
(define (render gamestate)
  (place-image
   player-image
   (posn-x (player1-posn (gamestate-player1 gamestate)))
   (posn-y (player1-posn (gamestate-player1 gamestate)))
   (render-map (gamestate-layout gamestate))))




;gamestate is a structure
(define-struct gamestate [layout bomb player1 player2 roundtimer maximum] #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;layout:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;layout:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;data types:
;layout is one of the following:
; -- (Vector of (Vector of Symbol)) 
; -- #false

;;;;;;;;;;;;;;;;;interpretation for (Vector of (Vector of Symbol));;;;;;;;;;;;;;;;;;;;;;;

;the layout of the game is a 2D vector grid composed of same-size-square-shape cells.
;each cell contains a symbol.
;each symbol represents a specific symbol in the game. 


;for each symbol:
;'S represents the safe area for players join the game at the start
;'W represents the walkable cell
;'I represents the indestructible cell
;'D represents the destructible cell
;'B represents the cell with unexploded bomb
;'E represents the cell with exploding bomb

; additional notes:
; -- each game has a random layout,there will be a function(generate-layout base)
; to randomizes the symbols 'W in base to create initial-layout
; -- in fact , 'S and 'W are both walkable cells.
; but , in the (generate-layout base) function, which randomizes the symbols 'W 
; 'S cells remain unchanged to provide a fixed safe area for players starting the game.
; -- similar , 'I is also unchangeble in the (generate-layout base) function

;examples:


;(define initial-layout (generate-layout base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;interpretation for #false;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;#false represents the the layout before game start
;for example, in start page of the game

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;layout:;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;layout:;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bomb ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bomb ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;data types:
;bomb is one of the following:
; -- '()  ;; no existing boomstate in game
; -- list of bombstate structure

;interpretation
;represents all of the existing bombstate
;including both exploding and unexploed bomb

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bombstate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;bombstate is a structure
(define-struct bombstate [cor countdown] #:transparent)
;interpretation:

;;;;;;;;;;;;;;;;;;;;;;;;;;;; countdown ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;countdown is a Integer in the interval
;from 5(included)
;to 0(included)

;interpretation:
;represents the countdown of the each bombstate
;5 is the time for bomb just added to game 
;2 is the time for bomb start to explode
;0 is the time for bomb end explode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;cor is a structure
(define-struct cor [column row] #:transparent)
;(make-cor Integer Integer)

;interpretation:
;represents the cell location in the game
;column is a Integer , represents the location of column
;row is a Integer, represents the location of row

;examples
;data example for cor:
(define first-row-and-column (make-cor 0 0))
;data example for bombstate:
(define added-bomb-to-00 (make-bombstate first-row-and-column 5))
;data exmample for bomb
(define initial-bomb '())
(define one-bomb (list added-bomb-to-00))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;bomb;;;;;;;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;bomb;;;;;;;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player1:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player1:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;data types:
;player1 is a structure
(define-struct player1 [posn dead? bombcount] #:transparent)
;(make-player1 posn Boolean Number)

;interpretation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;posn:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Posn represents the position of player1
;it is a precise position , different from the cor (row and column location)

;additional notes:
;to achieve some functions such as put-bomb/check-dead?
;use (round posn) this kind of function
;which convert posn -> cor to get the nearest cor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;dead?:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;dead? is a Boolean
;represents whether player1 has dead
;#t represents this player1 died
;#f represents this player1 alive

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;bombcount:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;bombcount is a Number
;represents the the amounts of the bomb that player1 put in the game
;and still exist (unexploded or exploding)


;example
(define initial-player1 (make-player1
                        (make-posn 0 0)
                        #false
                        0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player1;;;;;;;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player1;;;;;;;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;














;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;data types:
;player2 is a structure
(define-struct player2 [posn dead? bombcount] #:transparent)
;(make-player2 posn Boolean Number)

;interpretation is as similar as player1
;just replace all "player1" with "player2"

;example
(define initial-player2 (make-player1
                        (make-posn 200 200)
                        #false
                        0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player2;;;;;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;player2;;;;;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;














;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;roundtimer:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;roundtimer:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;roundtimer is a Interger in the interval:
;from 120(included)
;to 0(included)

;interpretation
;represents the countdown of one game

;examples;
(define initial-roundtimer 120)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;roundtimer;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;roundtimer;;;;;;;end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;maximum;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;maximum;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data types:
;maximum is a Interger in the interval:
;from 3(included)
;to 6(included)

;interpretation
;represents the maximum amounts of bomb that each player can put
;the bombcount of each player should less than or equal maximum

;additional note:
;it is not a fixed constant
;it will raise along with the roundtimer countdowns in one game
;to reduce the probability of tie.

;examples
(define initial-maximum 3)






;;;;;;;;;;;example of the gamestate;;;;;;;;;;;;;;;;;;;;
;(define initial-gamestate
;  initial-layout
;  initial-player1
;  initial-player2
;  initial-roundtimer
;  initial-maximum)




;(define-struct gamestate [layout bomb player1 player2 roundtimer maximum])



;cor layout -> symbol
(define (get-symbol layout cor)
  (vector-ref
   (vector-ref layout (cor-row cor))
   (cor-column cor)))



;;on-tick
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;no problem;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;cor layout -> Boolean
;为了扩展性，不使用硬编码
(define (in-bound? layout cor)
  (let (
        [max-row (- (vector-length layout) 1)]
        [max-column (- (vector-length (vector-ref layout 0)) 1)]
        )
  (and (<= 0 (cor-row cor) max-row)
       (<= 0 (cor-column cor) max-column))))

;cor layout -> Boolean
(define (check-U? layout cor)
  (eq? (get-symbol layout cor) 'U))

;cor layout -> Boolean
(define (check-D? layout cor)
  (eq? (get-symbol layout cor) 'D))

;convert:
;data types
;layout is a vector
;cor is a cor or list of cor
(define (convert layout cor symbol)
  (cond
    [(empty? cor) layout]
    [(list? cor)
    (convert
     (convert layout (first cor) symbol)
     (rest cor)
    symbol)]
    [else
     (begin
       (vector-set!
        (vector-ref layout
                    (cor-row cor))
        (cor-column cor)
        symbol)
       layout)]))

;constant definitions
(define BOOM-MAX-DISTANCE 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;no problem;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;boom-range;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;acc *****
(define (extend-direction direction-fn layout)
  (let loop ([n 1] [acc '()])
    (cond
      [(> n BOOM-MAX-DISTANCE) acc]
      [else
       (let (
             [new-cor (direction-fn n)]
             )
         (cond
           [(not (in-bound? layout new-cor)) acc]
           [(check-U? layout new-cor) acc]
           [(check-D? layout new-cor) (cons new-cor acc)]
           [else (loop (add1 n) (cons new-cor acc))]))])))


;boom-range:
;list<bombstate> layout -> list<cor>
;assume extent in n cells
;meet 'U , stop
;meet 'D , involve 'D but stop
;试图先生成potential-list-of-cor，发现不行，因为正确的逻辑是
;遇到就停，而不是先生成再筛选
;每个方向的扩展是独立的
(define (single-boom-range bombstate layout)
  (let* (
         [center (bombstate-cor bombstate)]
         
         [directions
          (list
           (lambda (n) (make-cor (cor-column center) (- (cor-row center) n)))
           (lambda (n) (make-cor (cor-column center) (+ (cor-row center) n)))
           (lambda (n) (make-cor (- (cor-column center) n) (cor-row center)))
           (lambda (n) (make-cor (+ (cor-column center) n) (cor-row center))))]

         [center-list (list center)]

         [ranges
          (map (lambda (dir-fn) (extend-direction dir-fn layout))
               directions)])
    (append center-list (apply append ranges))))

; boom-range: list<bombstate> layout -> list<cor>
(define (boom-range bomb-list layout)
  (apply append
         (map (lambda (bomb)
                (single-boom-range bomb layout))
              bomb-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;boom-range;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;boom;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;gamestate -> gamestate 
;question
;*****这里想想能不能用递归实现
;*****想想怎么把调用的boom-range传递给boom-end
(define (boom gamestate)
  (let* (
         [bomb-list (gamestate-bomb gamestate)]
         [layout (gamestate-layout gamestate)]

         ;get the countdown=2 bomb-list
         [exploding-list
          (filter
           (lambda(bombstate)
             (= (bombstate-countdown bombstate) 2))
             bomb-list)]

         ;calculate boom-range of exploding-list
         [initial-boom-cor (boom-range exploding-list layout)]

         ;apply chain-explosion to get the final-exploding-list
         [final-exploding-list
          (chain-explosion initial-boom-cor bomb-list layout)]

         ;get the countdown=2 bomb-list agagin
         [updated-exploding-list
          (filter
           (lambda(bombstate)
             (= (bombstate-countdown bombstate) 2))
           final-exploding-list)]

         ;calculate boom-range agagin
         [updated-boom-cor (boom-range updated-exploding-list layout)]

         ;calculate final-boom-cor
         [final-boom-cor (append initial-boom-cor updated-boom-cor)]

         ;;change the layout
         [new-layout (convert layout
                              final-boom-cor
                            'T)]
         
         ;;get the new gamestate
         [new-gamestate
          (make-gamestate
           new-layout
           final-exploding-list
           (gamestate-player1 gamestate)
           (gamestate-player2 gamestate)
           (gamestate-roundtimer gamestate)
           (gamestate-maximum gamestate))]
         )
    new-gamestate))

;chain-explosion:
;list-of-cor bomb-list layout -> bomb-list
;member不能比较两个structure，需要自定义function
;*****
;可能是计时器会约等于？？所以时间接近会都算2秒
;因此不能添加not = 2 约束chain-explosion
;考虑用别的方法约束，比如自己不算chain-explosion
(define (chain-explosion initial-boom-cor bomb-list layout)
    (map
     (lambda (bombstate)
             (if 
               (in-boom-cor? (bombstate-cor bombstate) initial-boom-cor)

              (begin
                (printf "炸弹位置 ~a 受chain-explosion影响,倒计时重置为 2。\n" (bombstate-cor bombstate))
                (make-bombstate (bombstate-cor bombstate) 2))
              (begin
                (printf "炸弹位置 ~a 没受chain-explosion影响 ~a。\n" (bombstate-cor bombstate) (bombstate-countdown bombstate))
                bombstate)))
     
              bomb-list)
    )

;cor cor -> Boolean
(define (cor=? cor1 cor2)
  (and (= (cor-column cor1) (cor-column cor2))
       (= (cor-row cor1) (cor-row cor2))))
  
;cor list<cor> -> Boolean
(define (in-boom-cor? cor cor-list)
  (cond
    [(empty? cor-list) #f]
    [else
     (if
      (cor=? cor (first cor-list))
      #t
      (in-boom-cor? cor (rest cor-list)))]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;boom;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;boom-end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;gamestate -> gamestate
;boom-end convert the 'T(boom-range) -> 'W (layout)
;boom-end remove the countdown=0 bomb (bomb)
;boom-end remove the bombcount of owner player (player1 and player2)

;question0
;整体逻辑理顺
;boom-end接受的是updated-gamestate，然后把layout和bomb传递给boom-range来计算爆炸范围
;他会把boom-range内的单元格重置为'W
;同时移除倒计时为0的炸弹
;同时移除拥有者的bombcount（这里多半要改,所以先不写）
;因此，他需要调用boom-range以及辅助函数，以及remove-bomb三个辅助函数实现功能





(define (boom-end gamestate)
  (let* (
         [bomb-list (gamestate-bomb gamestate)]
         [layout (gamestate-layout gamestate)]

         ;get the countdown=0 bomb
         [exploded-bomb
          (filter
           (lambda(bombstate)
             (= (bombstate-countdown bombstate) 0))
             bomb-list)]


         ;;change the layout
         [new-layout (convert layout
                              (boom-range exploded-bomb layout)
                              'W)]
                           

         ;;change the bomb-list
         [new-bomb-list (remove-bomb bomb-list)]

         ;;get the new gamestate
         [new-gamestate
          (make-gamestate
           new-layout
           new-bomb-list
           (gamestate-player1 gamestate)
           (gamestate-player2 gamestate)
           (gamestate-roundtimer gamestate)
           (gamestate-maximum gamestate))]
         )
   ; (begin
   ;   (printf "倒计时 ~a \n"
      (gamestate-roundtimer new-gamestate)
      new-gamestate))
      
;list<bombstate> -> list<bombstate>
;convert new bomb list with every countdown > 0
(define (remove-bomb lob)
  (let* (
         [removed-bombs (filter (lambda (bomb)
                                   (<= (bombstate-countdown bomb) 0)) lob)]
         [remaining-bombs (filter (lambda (bomb)
                                    (> (bombstate-countdown bomb) 0)) lob)]
         )
    (begin
    (printf "炸弹列表 ~a 被remove-bomb移除。\n" removed-bombs)
    remaining-bombs)))
;;这个函数没问题，有问题去找别的


            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;boom-end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                             

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;update timer part;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Number -> Number
(define (updated-roundtimer roundtimer)
  (max 0 (sub1 roundtimer)))

;bomb -> bomb
(define (updated-bomb bomb)
  (begin
  (map (lambda (bombstate)
         (make-bombstate
          (bombstate-cor bombstate)
          (max 0 (sub1 (bombstate-countdown bombstate)))))
       bomb)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;update timer part;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;roundtimer and maximum;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;这里注意初始值设为2，或者改这个函数*****
;Number -> Boolean
(define (check-roundtimer? roundtimer)
  (= (modulo roundtimer 30) 0))

;Number -> Number
(define (add-maximum maximum)
  (min 6 (add1 maximum)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;roundtimer and maximum;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;check-zero? and two?;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;gamestate -> Boolean
;list<bomb> -> Boolean
(define (check-zero? lob)
  (cond
    [(empty? lob) #f]
    [else
     (if
     (= (bombstate-countdown (first lob)) 0)
     #t
     (check-zero? (rest lob)))]))


;list<bomb> -> Boolean
(define (check-two? lob)
  (cond
    [(empty? lob) #f]
    [else
     (if
     (= (bombstate-countdown (first lob)) 2)
     #t
     (check-two? (rest lob)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;check-zero? and two?;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
              
;gamestate -> gamestate
(define (timehandler gamestate)
  (let*
      (
       [updated-gamestate
        (make-gamestate  (gamestate-layout gamestate)
                         (updated-bomb (gamestate-bomb gamestate))
                         (gamestate-player1 gamestate)
                         (gamestate-player2 gamestate)
                         (updated-roundtimer (gamestate-roundtimer gamestate))
                         (if (check-roundtimer? (gamestate-roundtimer gamestate))
                                                (add-maximum (gamestate-maximum gamestate))
                                                (gamestate-maximum gamestate)))]
       ;;update game state (update timer for roundtimer and bomb-countwdown)
       ;;and decide if add-maximum
       [boom-end-state
        (if (check-zero? (gamestate-bomb updated-gamestate))
            (boom-end updated-gamestate)
            updated-gamestate)]
       ;;handle countdown-zero bomb
       [bomb-boom-state
        (if (check-two? (gamestate-bomb boom-end-state))
            (boom boom-end-state)
            boom-end-state)]
       
       
       ;;handle countdown-two bomb
       )
    bomb-boom-state))

;question0
;总体逻辑理顺
;先更新的游戏状态（更新计时器，决定要不要增加最大炸弹数量）
;查看新游戏状态下的炸弹列表，check-zero决定要不要执行boom-end
;查看返回的新游戏状态，决定要不要执行boom
;新的游戏状态通过四个辅助函数（更新炸弹，更新计时器，检查计时器是否为30的整数，增加maximum）实现
;boom-end-state状态通过两个辅助函数（检查有没有炸弹倒计时为0，处理boom-end（改layout为’W并且移除炸弹））实现
;boom-boom-state通过两个辅助函数（检查有没有炸弹倒计时为2，处理boom（改layout为‘T并且让范围内炸弹为2））实现

;question1
;为什么不用cond？
;cond是遇到对的直接执行
;最初的方案是cond先zero再two规避掉问题
;但逻辑上这样是不对的，cond不适合同时处理多个条件的任务
;因此改为let*和if不断传递新gamestate再handle

;question2
;为什么要挨个传递状态而不是统一用updated-gamestate？
;updated-gamestate 0 1 2
;boom-end-state 1 2
;updated-gamestate是0 1 2倒计时的炸弹,
;boom-end则是 1 2 如果check-two调用updated-gamestate,
;然后正好check-two check到了一个炸弹
;可能会引爆那个倒计时是0但是还没移除的炸弹,导致错误的二次爆炸

;question3
;为什么要先处理zero再处理two？ *****尊嘟假嘟啊 我感觉是这样但没试
;假设有炸弹倒计时分别为3 1 2
;update-gamestate之后变成2 0 1
;如果我们先处理check-two得到new-gamestate
;2会爆炸，如果0也在范围内，由于chain-exposion，会被重置倒计时为2
;导致错误的二次爆炸
;而如果是先zero再two
;当接收到updated-gamestate为2 0 1时
;一定会先处理掉0，从而返回一个新countdown 2 1 的炸弹列表
;这是check-two依然可以正确处理2，而不会导致错误的二次爆炸。

;question4
;怎么处理roundtimer和maximum之间的交互？
;由于这二者是独立于bomb的field，因此直接在updated-gamestate里用if更新









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;keyhandler
;gamestate ke -> gamestate
;up down left right only change the position of player

;auxiliary functions

;Posn -> Coordinate
(define (nearest-cor posn)
  (make-cor
   (floor (/ (posn-x posn) CELL-SIZE))
   (floor (/ (posn-y posn) CELL-SIZE))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;move;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;gamestate -> gamestate
;****这一坨优化一下，考虑直接在主函数里处理字还少点
(define (move-down gamestate)
  (make-gamestate
   (gamestate-layout gamestate)
   (gamestate-bomb gamestate)
   (make-player1 (make-posn
                  (posn-x
                   (player1-posn
                    (gamestate-player1 gamestate)))
                  (+ (posn-y
                      (player1-posn
                       (gamestate-player1 gamestate)))
                     velocity))
                 #f
                  (player1-bombcount (gamestate-player1 gamestate)))         
   (gamestate-player2 gamestate)
   (gamestate-roundtimer gamestate)
   (gamestate-maximum gamestate)))

(define (move-up gamestate)
  (make-gamestate
   (gamestate-layout gamestate)
   (gamestate-bomb gamestate)
   (make-player1 (make-posn
                  (posn-x
                   (player1-posn
                    (gamestate-player1 gamestate)))
                  (- (posn-y
                      (player1-posn
                       (gamestate-player1 gamestate)))
                     velocity))
                 #f
                  (player1-bombcount (gamestate-player1 gamestate)))         
   (gamestate-player2 gamestate)
   (gamestate-roundtimer gamestate)
   (gamestate-maximum gamestate)))

(define (move-left gamestate)
  (make-gamestate
   (gamestate-layout gamestate)
   (gamestate-bomb gamestate)
   (make-player1 (make-posn
                  (- (posn-x
                   (player1-posn
                    (gamestate-player1 gamestate)))
                     velocity)
                  (posn-y
                      (player1-posn
                       (gamestate-player1 gamestate))))
                  #f
                  (player1-bombcount (gamestate-player1 gamestate)))        
   (gamestate-player2 gamestate)
   (gamestate-roundtimer gamestate)
   (gamestate-maximum gamestate)))

(define (move-right gamestate)
  (make-gamestate
   (gamestate-layout gamestate)
   (gamestate-bomb gamestate)
   (make-player1 (make-posn
                  (+ (posn-x
                   (player1-posn
                    (gamestate-player1 gamestate)))
                     velocity)
                  (posn-y
                      (player1-posn
                       (gamestate-player1 gamestate)))) 
                 #f
                  (player1-bombcount (gamestate-player1 gamestate)))       
   (gamestate-player2 gamestate)
   (gamestate-roundtimer gamestate)
   (gamestate-maximum gamestate)))

;; move-predicate?: gamestate cor -> Boolean

(define (move-predicate? layout current-cor new-cor)
  (let (
        [current-symbol (get-symbol layout current-cor)]
        [new-symbol (get-symbol layout new-cor)]
        )
  (and (<= 0 (cor-column new-cor) 14)   
       (<= 0 (cor-row new-cor) 10)
       (or
        (symbol=? new-symbol 'W)
        (symbol=? new-symbol 'S)
        (symbol=? current-symbol 'B)
        (symbol=? new-symbol 'T)))))


;这里移动预测实现了放完炸弹可以从炸弹上下去
;但是还要加上new-symbol不能是墙/另一个炸弹，以符合大多数bomberman游戏规则
;但是需要更改近似函数和velocity让预测更加精准，后面解决*****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;move;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;put-bomb;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;cor
(define (put-predicate? layout current-cor)
  (let (
        [current-symbol (get-symbol layout current-cor)]
        )
    (or (symbol=? current-symbol 'W)
        (symbol=? current-symbol 'S))))


;gamestate -> gamestate
(define (put-bomb gamestate current-cor)
    (make-gamestate
     (convert (gamestate-layout gamestate) current-cor 'B) 
     (cons (make-bombstate current-cor 5)                  
           (gamestate-bomb gamestate))
     (gamestate-player1 gamestate)
     (gamestate-player2 gamestate)
     (gamestate-roundtimer gamestate)
     (gamestate-maximum gamestate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;put-bomb;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                        
;; move: gamestate key -> gamestate
(define (keyhandler gamestate ke)
  (let* (
         [layout (gamestate-layout gamestate)]
         [current-posn-player1 (player1-posn (gamestate-player1 gamestate))]

         [posn-x-player1 (posn-x current-posn-player1)]
         [posn-y-player1 (posn-y current-posn-player1)]

         [current-cor (nearest-cor current-posn-player1)]
         )
         
    (cond
      [(string=? ke "down")
       (let*(
             [new-posn (make-posn posn-x-player1 (+ posn-y-player1 velocity))]

             [new-cor (nearest-cor new-posn)]
             )
       (if (move-predicate? layout current-cor new-cor)
           (move-down gamestate)
           gamestate))]
      [(string=? ke "up")
       (let*(
             [new-posn (make-posn posn-x-player1 (- posn-y-player1 velocity))]

             [new-cor (nearest-cor new-posn)]
             )
       (if (move-predicate? layout current-cor new-cor)
           (move-up gamestate)
           gamestate))]
      [(string=? ke "left")
       (let*(
             [new-posn (make-posn (- posn-x-player1 velocity) posn-y-player1)]

             [new-cor (nearest-cor new-posn)]
             )
       (if (move-predicate? layout current-cor new-cor)
           (move-left gamestate)
           gamestate))]
      [(string=? ke "right")
       (let*(
             [new-posn (make-posn (+ posn-x-player1 velocity) posn-y-player1)]

             [new-cor (nearest-cor new-posn)]
             )
       (if (move-predicate? layout current-cor new-cor)
           (move-right gamestate)
           gamestate))]
      [(string=? ke "a")
       (if (put-predicate? layout current-cor)
           (put-bomb gamestate current-cor)
           gamestate)]
      [else gamestate])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;keyhandler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


;main function
(define (main gamestate)
  (big-bang gamestate
    [to-draw render]
    [on-key keyhandler]
    [on-tick timehandler 1]))
    
  
     

             
(define initial-state
  (make-gamestate
   layout1
   '()
   (make-player1 (make-posn 10 10) #f 0)
   (make-player2 (make-posn 100 100) #f 0)
   120
   3))


;application
(main initial-state)










