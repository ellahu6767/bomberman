;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |boom-range another solution|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;bombstate -> list of cor
(define (single-boom-range bombstate layout)
  (let* (
        [center (bombstate-cor bombstate)]
        [cor-x (cor-column center)]
        [cor-y (cor-row center)]

        [potential-boom-range-except-center
         (list
         ;;right
         (list (make-cor (+ cor-x 1) cor-y)
               (make-cor (+ cor-x 2) cor-y))
         ;;left
         (list (make-cor (- cor-x 1) cor-y)
               (make-cor (- cor-x 2) cor-y))
         ;;down
         (list (make-cor cor-x (+ cor-y 1))
               (make-cor cor-x (+ cor-y 2)))
         ;;up
         (list (make-cor cor-x (- cor-y 1))
               (make-cor cor-x (- cor-y 2))))]

        [boom-range-except-center
         (map
          (lambda(potential-list)
            (extend-direction potential-list layout))
          potential-boom-range-except-center)]
        )
    (cons center
          (apply append boom-range-except-center))))


;list of cor -> list of cor
(define (extend-direction potential-list layout)
  (cond
    [(or
      (empty? potential-list)
      (not (in-bound? (first potential-list))))
     '()]

    [(check-I? layout (first potential-list)) '()]

    [(check-D? layout (first potential-list))
     (list (first potential-list))]

    [else
     (cons (first potential-list)
           (extend-direction (rest potential-list) layout))]))

 
       

      
         


         

  
         
    
         
        
    