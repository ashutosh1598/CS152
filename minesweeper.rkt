#lang racket
(require racket/gui)
(require racket/draw)
(define score 0)
(define x 9)
(define y 9)
(define size 60)
(define number-of-mines 10)
(define (all-true p l)
  (cond[(null? l) #t]
       [(p (car l)) (all-true p (cdr l))]
       [else #f]))
(define (update-score x)
  (begin (set! score (+ score x))
               (send dc set-pen "Dark Slate Gray" 1 'solid)
               (send dc set-brush "Dark Slate Gray" 'solid)
               (send dc draw-rectangle (* size 1.25) (/ size 4) size size)
               (send dc set-text-foreground "Honeydew")
               (send dc draw-text (number->string score) (* size 1.25) (/ size 4))))
(define (f t)
  (set-field! neighbour-mine-count
              (vector-ref game-cells t)
              (number-of-trues (lambda(x)(get-field HasMine x))
                               (surrounding-cells (vector-ref game-cells t)))))

(define (number-of-trues p l)
  (cond[(null? l) 0]
       [(p (car l))(+ 1 (number-of-trues p (cdr l)))]
       [else (number-of-trues p (cdr l))]))
(define (h f l)
  (if(null? (cdr l)) (f (car l))
     (begin (f (car l)) (h f (cdr l)))))
     
(define frame (new frame%
                   [label "Minesweeper"]
                   [width 800]
                   [height 800]))


(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      
       (cond[(eq?(send event get-event-type) 'left-down)
             (let*[(a (- (send event get-x) (/ (- 800 (* size y)) 2) ))
                   (b (- (send event get-y) (/ (- 800 (* size x)) 2)))
                  (posn (+ (* y (quotient b size)) (quotient a size)))]
            (left-click (vector-ref game-cells posn)))]
            [(eq? (send event get-event-type) 'right-down)
             (let*[(a (- (send event get-x) (/ (- 800 (* size y)) 2)))
                   (b (- (send event get-y) (/ (- 800 (* size x)) 2)))
                  (posn (+ (* y (quotient b size)) (quotient a size)))]
            (right-click (vector-ref game-cells posn)))]))

      (super-new)))
(define (grid dc)
  (let*[(X (/ (- 800 (* size y)) 2))
        (Y (/ (- 800 (* size x)) 2))]
  (begin (send dc set-brush "Cadet Blue"'solid)
         (send dc draw-rectangle X Y (* size y) (* size x))
         (send dc set-brush "Dark Slate Gray" 'solid)
         (define (hor-lines i)
           (cond[(< i (- x 1)) (begin (send dc draw-line X (+ Y (* size (+ 1 i))) (- 800 X) (+ Y (* size (+ 1 i))))
                                      (hor-lines (+ i 1)))]))
         (define (ver-lines i)
           (cond[(< i (- y 1)) (begin (send dc draw-line (+ X (* size (+ 1 i))) Y (+ X (* size (+ 1 i))) (- 800 Y))
                                      (ver-lines (+ i 1)))]))
         (hor-lines 0)
         (ver-lines 0))))
(define game-canvas (new my-canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-brush "Dark Slate Gray" 'solid)
                (send dc draw-rectangle 0 0 800 800)
                (grid dc)
                (send dc set-text-foreground "Honeydew")
                (send dc set-font (make-font #:size (/ size 3) #:family 'roman
                             #:weight 'bold))
                (send dc draw-text "Score  0" 0 (/ size 4))
                
                )]))
(define dc (send game-canvas get-dc))
                          
(send frame show #t)


(define cell%
  (class object%
    (init-field  row col Isopen HasFlag HasMine neighbour-mine-count )
    (super-new)))

(define game-cells (make-vector (* x y)))
(define (create-game-cells i)
  (cond[(< i (* x y)) (begin (vector-set! game-cells i (new cell%
                                                        [row (quotient i y)]
                                                        [col (remainder i y)]
                                                        [Isopen #f]
                                                        [HasFlag #f]
                                                        [HasMine #f]
                                                        [neighbour-mine-count 0]))
                         
                         (create-game-cells (+ i 1)))]))

(define (generating-mines j)
  (cond[(< j number-of-mines) (begin (define n (random (* x y)))
                        (set-field! HasMine (vector-ref game-cells n) #t)
                        (generating-mines (+ j 1)))]))


(define (surrounding-cells single-cell)
  (let*[(posn (+ (* y (get-field row single-cell)) (get-field col single-cell)))]
    (cond[(= posn 0) (list (vector-ref game-cells 1)
                           (vector-ref game-cells y)
                           (vector-ref game-cells (+ y 1)))]
         [(= posn (- y 1)) (list (vector-ref game-cells (- y 2))
                                 (vector-ref game-cells (+ posn y))
                                 (vector-ref game-cells (+ posn (- y 1))))]
                                       
         [(= posn (* (- x 1) y)) (list (vector-ref game-cells (+ posn 1))
                                       (vector-ref game-cells (- posn y))
                                       (vector-ref game-cells (- posn (- y 1))))]
                                       
         [(= posn (- (* x y) 1)) (list (vector-ref game-cells (- posn 1))
                                       (vector-ref game-cells (- posn y))
                                       (vector-ref game-cells (- posn y 1)))]
                                                               
         [(and (< posn (- y 1)) (> posn 0))
          (list (vector-ref game-cells (- posn 1))
                (vector-ref game-cells (+ posn 1))
                (vector-ref game-cells (+ posn (- y 1)))
                (vector-ref game-cells (+ posn y))
                (vector-ref game-cells (+ posn (+ y 1))))]
                                                      
         [(and (< posn (- (* x y) 1)) (> posn (* (- x 1) y)))
          (list (vector-ref game-cells (- posn 1))
                (vector-ref game-cells (+ posn 1))
                (vector-ref game-cells (- posn y 1))
                (vector-ref game-cells (- posn y))
                (vector-ref game-cells (- posn (- y 1))))]
                                                      
         [(= 0 (remainder posn y))
          (list (vector-ref game-cells (- posn (- y 1)))
                (vector-ref game-cells (+ posn 1))
                (vector-ref game-cells (+ posn y))
                (vector-ref game-cells (- posn y))
                (vector-ref game-cells (+ posn y 1)))]
                                                     
         [(= (- y 1) (remainder posn y))
          (list (vector-ref game-cells (- posn y))
                (vector-ref game-cells (- posn y 1))
                (vector-ref game-cells (- posn 1))
                (vector-ref game-cells (+ posn (- y 1)))
                (vector-ref game-cells (+ posn y)))]
                                                      
         [else (list (vector-ref game-cells (- posn 1))
                     (vector-ref game-cells (+ posn 1))
                     (vector-ref game-cells (+ posn y))
                     (vector-ref game-cells (+ posn y 1))
                     (vector-ref game-cells (+ posn (- y 1)))
                     (vector-ref game-cells (- posn y 1))
                     (vector-ref game-cells (- posn y))
                     (vector-ref game-cells (- posn (- y 1))))])))


                       

(define (calc-neigh-mine-count i)
  (cond[(< i (* x y)) (cond[(= i 0)
                            (begin (f i)
                                   (calc-neigh-mine-count (+ i 1)))]
                           [(= i (- y 1))
                            (begin (f i)
                                   (calc-neigh-mine-count (+ i 1)))]
                           [(= i (* (- x 1) y))
                            (begin (f i)
                                   (calc-neigh-mine-count (+ i 1)))]
                           [(= i (- (* x y) 1))
                            (begin (f i)
                                   (calc-neigh-mine-count (+ i 1)))]
                           [(and (< i (- y 1)) (> i 0))
                            (begin (f i)
                                   (calc-neigh-mine-count (+ i 1)))]
                           [(and (< i (- (* x y) 1)) (> i (* (- x 1) y)))
                            (begin (f i)
                                   (calc-neigh-mine-count (+ i 1)))]
                           [(= 0 (remainder i y))
                            (begin (f i)
                                   (calc-neigh-mine-count (+ i 1)))]
                           [(= (- y 1) (remainder i y))
                            (begin (f i)
                                   (calc-neigh-mine-count (+ i 1)))]
                           [else
                            (begin (f i)
                                   (calc-neigh-mine-count (+ i 1)))])]))
(create-game-cells 0)
(generating-mines 0)
(calc-neigh-mine-count 0)


(define (draw-flag x y)
  (begin (send dc set-pen "Red" 4 'solid)
         (send dc draw-line (+ x (/ size 3)) (+ y (/ size 4)) (+ x (/ size 3)) (+ y (* 3 (/ size 4))))
         (send dc draw-line (+ x (/ size 3)) (+ y (/ size 4)) (+ x (* (/ size 3) 2)) (+ y (* 1.5 (/ size 4))))
         (send dc draw-line (+ x (/ size 3)) (+ y (/ size 2)) (+ x (* (/ size 3) 2)) (+ y (* 1.5 (/ size 4))))))


        
  
        
(define (left-click single-cell)
  (cond[(and (not (get-field Isopen single-cell)) (not (get-field HasFlag single-cell)))
        (let*[(X (+ (/ (- 800 (* size y)) 2) (* size (get-field col single-cell))))
              (Y (+ (/ (- 800 (* size x)) 2) (* size (get-field row single-cell))))]
          (cond
            [(get-field HasMine single-cell)  (begin (send dc erase)
                                                     (send dc set-font (make-font #:size 40 #:family 'roman
                                                                                  #:weight 'bold))
                                                     (send dc set-text-foreground "Brown")
                                                     (send dc draw-text "GAME OVER" 240 330))]
            [(= 0 (get-field neighbour-mine-count single-cell))
             (let*[(r (get-field row single-cell))
                   (c (get-field col single-cell))
                   (l (surrounding-cells single-cell))]
               (begin (set-field! Isopen single-cell #t )
                      (send dc set-brush "Honeydew"  'solid)
                      (send dc set-pen "Black" 1 'solid)
                      (send dc draw-rectangle X Y size size)
                            
                      (update-score 10)
                      (h left-click l)))]
             
        [else (begin (send dc set-text-foreground "Dark Slate Gray")
                     (send dc set-brush "Honeydew"  'solid)
                     (send dc set-pen "Black" 1 'solid)
                     (send dc set-font (make-font #:size (/ size 3) #:family 'roman
                             #:weight 'bold))
                     (send dc draw-rectangle X Y size size)
                           
                     (send dc draw-text (number->string
                                         (get-field neighbour-mine-count single-cell))
                           (+ X (* 3 (/ size 8)))
                           (+ Y (/ size 4)))
                     (update-score 10)
                     (set-field! Isopen single-cell #t))]))]))

(define (right-click single-cell)
  (cond [(not (get-field Isopen single-cell))
         (let*[(X (+ (/ (- 800 (* size y)) 2) (* size (get-field col single-cell))))
               (Y (+ (/ (- 800 (* size x)) 2) (* size (get-field row single-cell))))]
           (cond [(not (get-field HasFlag single-cell))
                  (begin (send dc set-pen "Black" 1 'solid)
                         (send dc set-brush "Yellow" 'solid)
                         (send dc draw-rectangle X Y size size)
                         (draw-flag X Y)
                         (update-score 50)
                         (set-field! HasFlag single-cell #t)
                         (if(all-true (lambda(x) (equal? (get-field HasMine x)
                                                         (get-field HasFlag x)))
                                      (vector->list game-cells))
                            (begin (send dc erase)
                                   (send dc set-text-foreground "Brown")
                                   (send dc set-font (make-font #:size 35 #:family 'roman
                                                                #:weight 'bold))
                                   (send dc draw-text "CONGRATULATIONS YOU WON" 20 350)) (void)))]
                 [else (begin (send dc set-brush "Cadet Blue" 'solid)
                              (send dc set-pen "white" 1 'transparent)
                              (send dc draw-rectangle (+ X 1) (+ Y 1) (- size 1) (- size 1))
                              (update-score (* 50 -1))       
                              (set-field! HasFlag single-cell #f))]))]))                    

 
  
        
                
                
              
  
  
  
  
  
  
                                            
  
     
     
   

                          