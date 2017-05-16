;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname TetrisV1.0Tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define grid-scale 25)
(define BG-W 10)
(define BG-L 20)

;; Number -> Image
;; creates and image of n outline squares next to each other

(check-expect (square-line 3) (beside (square grid-scale "outline" "black")
                                      (square grid-scale "outline" "black")
                                      (square grid-scale "outline" "black")))
(check-expect (square-line 0) empty-image)

(define (square-line anum)
  (cond [(> anum 0) (beside (square grid-scale "outline" "black")
                            (square-line (sub1 anum)))]
        [else empty-image]))

;; Number number -> Image
;; creates an image of n rows of x outline squares next to each other

(check-expect (grid-create 3 4) (above (square-line 4)
                                       (square-line 4)
                                       (square-line 4)))
                                       
(check-expect (grid-create 0 4) empty-image)
(check-expect (grid-create 4 0) empty-image)

(define (grid-create n x)
  (cond [(> n 0) (above (square-line x)
                        (grid-create (sub1 n) x))]
        [else empty-image]))

(define BG (overlay (rectangle (* grid-scale BG-W)
                               (* grid-scale BG-L)
                               "outline" "black")
                    (grid-create BG-L BG-W)))

; Tetris game will be played in a 10 x 20 grid.
; each grid unit is 25 pixels by 25 pixels


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions
 
;; A Block is a (make-block Number Number Color)
(define-struct block (x y color))

;; block-temp: Block -> ?
#;(define (block-temp ablock)
    (...(block-x ablock)...
        (block-y ablock)...
        (block-color ablock)...))

;; A Set of Blocks (BSet) is one of:
;; - empty
;; - (cons Block BSet)
;; Order does not matter.

;; bset-temp: Bset -> ?
#; (define (bset-temp abset)
     (cond [(empty? abset) ...]
           [(cons? abset) ...(block-temp (first abset))...
                          ...(bset-temp (rest abset))...]))

;; A Tetra is a (make-tetra Posn BSet)
;; The center point is the point around which the tetra rotates
;; when it spins.
(define-struct tetra (center blocks))

;;tetra-temp: Tetra -> ?
#;(define (tetra-temp atetra)
    (...(tetra-center atetra)...
        (tetra-blocks atetra)...))

(define O (make-tetra
           (make-posn 6 1)
           (list (make-block 5 1 "green")
                 (make-block 6 1 "green")
                 (make-block 5 2 "green")
                 (make-block 6 2 "green"))))

(define I (make-tetra
           (make-posn 4 1)
           (list (make-block 3 1 "blue")
                 (make-block 4 1 "blue")
                 (make-block 5 1 "blue")
                 (make-block 6 1 "blue"))))

(define L (make-tetra
           (make-posn 6 2)
           (list (make-block 6 1 "purple")
                 (make-block 4 2 "purple")
                 (make-block 5 2 "purple")
                 (make-block 6 2 "purple"))))

(define J (make-tetra
           (make-posn 4 2)
           (list (make-block 4 1 "aqua")
                 (make-block 4 2 "aqua")
                 (make-block 5 2 "aqua")
                 (make-block 6 2 "aqua"))))

(define T (make-tetra
           (make-posn 5 2)
           (list (make-block 5 1 "orange")
                 (make-block 4 2 "orange")
                 (make-block 5 2 "orange")
                 (make-block 6 2 "orange"))))

(define Z (make-tetra
           (make-posn 5 1)
           (list (make-block 4 1 "pink")
                 (make-block 5 1 "pink")
                 (make-block 5 2 "pink")
                 (make-block 6 2 "pink"))))
(define S (make-tetra
           (make-posn 5 1)
           (list (make-block 6 1 "red")
                 (make-block 5 1 "red")
                 (make-block 4 2 "red")
                 (make-block 5 2 "red"))))

;; A Set of Tetra (Tset) is a (list O I L J T Z S)

(define Tset (list O I L J T Z S))

;; A World is a (make-world Tetra BSet Number)
;; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile score))

(define world1 (make-world O empty 0))
(define world2 (make-world L (list (make-block 5 BG-L "green")
                 (make-block 6 BG-L "green")
                 (make-block 5 (- BG-L 1) "green")
                 (make-block 6 (- BG-L 1) "green")) 0))

;; block-contains? : Block Bset -> Boolean
;; returns true if a Block is taking up the same grid sapce
;; as another block within a given Bset
(check-expect (block-contains? (make-block 5 2 "red")
                         (list (make-block 6 1 "red")
                                (make-block 5 1 "red")
                                (make-block 4 2 "red")
                                (make-block 5 2 "red"))) true)

(check-expect (block-contains? (make-block 5 3 "green")
                         (list (make-block 6 1 "red")
                                (make-block 5 1 "red")
                                (make-block 4 2 "red")
                                (make-block 5 2 "red"))) false)

(define (block-contains? ablock abset)
  (ormap (λ (x) (and (= (block-x x) (block-x ablock))
                   (= (block-y x) (block-y ablock)))) abset))

;; bset-contains?: Bset Bset -> Boolean
;; returns true if any block within a Bset is taking up the gridspace
;; of any block within another Bset.

(check-expect (bset-contains?
               (list (make-block 4 1 "pink")
                     (make-block 5 1 "pink")
                     (make-block 5 2 "pink")
                     (make-block 6 2 "pink"))
               (list (make-block 6 1 "red")
                     (make-block 5 1 "red")
                     (make-block 4 2 "red")
                     (make-block 5 2 "red"))) true)

(check-expect (bset-contains?
               (list (make-block 4 1 "pink")
                     (make-block 5 1 "pink")
                     (make-block 5 2 "pink")
                     (make-block 6 2 "pink"))
               (list (make-block 6 10 "red")
                     (make-block 5 10 "red")
                     (make-block 4 10 "red")
                     (make-block 5 10 "red"))) false)

(define (bset-contains? abset1 abset2)
  (ormap (λ (x) (block-contains? x abset2)) abset1))

;; block-rotate-ccw : Posn Block -> Block
;; Rotate the block 90 counterclockwise around the posn.

(check-expect (block-rotate-ccw (make-posn 6 7) (make-block 6 8 "blue"))
              (make-block 5 7 "blue"))
(check-expect (block-rotate-ccw (make-posn 3 12) (make-block 4 7 "blue"))
              (make-block 8 13 "blue"))

(define (block-rotate-ccw c b)
  (make-block (+ (posn-x c)
                 (- (posn-y c)
                    (block-y b)))
              (+ (posn-y c)
                 (- (block-x b)
                    (posn-x c)))
              (block-color b)))

;; block-rotate-cw: Posn Block -> Block
;; Rotate the block 90 clockwise around the posn.

(check-expect (block-rotate-cw (make-posn 6 12) (make-block 4 7 "blue"))
              (make-block 1 14 "blue"))
(check-expect (block-rotate-cw (make-posn 9 18) (make-block 9 16 "blue"))
              (make-block 7 18 "blue"))

(define (block-rotate-cw c b)
  (block-rotate-ccw c (block-rotate-ccw c (block-rotate-ccw c b))))

;; bset-rotate-ccw: Posn Bset -> Bset
;; Rotates a Set of Blocks around a given Posn
(check-expect (bset-rotate-ccw (make-posn 4 4) empty)empty)
(check-expect (bset-rotate-ccw (make-posn 4 4) (list (make-block 4 7 "blue")
                                                     (make-block 4 8 "blue")))
              (list (make-block 1 4 "blue") (make-block 0 4 "blue")))

(define (bset-rotate-ccw aposn abset)
  (map (λ (x) (block-rotate-ccw aposn x)) abset))

;; bset-rotate-cw: Posn Bset -> Bset
;; Rotates a Set of Blocks around a given Posn

(check-expect (bset-rotate-cw (make-posn 4 4) empty)empty)
(check-expect (bset-rotate-cw (make-posn 4 4) (list (make-block 4 7 "blue")
                                                     (make-block 4 8 "blue")))
              (list (make-block 7 4 "blue") (make-block 8 4 "blue")))

(define (bset-rotate-cw aposn abset)
  (map (λ (x) (block-rotate-cw aposn x)) abset))

;; tetra-rotate-ccw: Tetra -> Tetra
;; Rotates a Tetra ccw around its center

(check-expect (tetra-rotate-ccw (make-tetra (make-posn 6 10)
           (list (make-block 5 10 "green")
                 (make-block 6 10 "green")
                 (make-block 5 11 "green")
                 (make-block 6 11 "green"))))
              (make-tetra (make-posn 6 10) (list (make-block 6 9 "green")
                                                 (make-block 6 10 "green")
                                                 (make-block 5 9 "green")
                                                 (make-block 5 10 "green"))))

(check-expect (tetra-rotate-ccw (make-tetra (make-posn 4 10)
           (list (make-block 3 10 "green")
                 (make-block 4 10 "green")
                 (make-block 3 11 "green")
                 (make-block 4 11 "green"))))
              (make-tetra (make-posn 4 10) (list (make-block 4 9 "green")
                                                 (make-block 4 10 "green")
                                                 (make-block 3 9 "green")
                                                 (make-block 3 10 "green"))))
(define (tetra-rotate-ccw atetra)
  (make-tetra (tetra-center atetra)
              (bset-rotate-ccw (tetra-center atetra)
                               (tetra-blocks atetra))))

;; rotate-cw? Posn Bset Bset -> Boolean
;; Takes in two Bsets an checks if the first Bset is able to
;; rotate clockwise around the given Posn:
;; Conditions:
;; - rotation of Bset cannot put Bset out of the screen
;; - rotation of Bset cannot overlap another Bset

(check-expect (rotate-cw? (make-posn 1 2)
           (list (make-block 1 1 "purple")
                 (make-block 1 2 "purple")
                 (make-block 1 3 "purple")
                 (make-block 1 4 "purple"))
           empty) false)

(check-expect (rotate-cw? (make-posn BG-W 2)
           (list (make-block BG-W 1 "purple")
                 (make-block BG-W 2 "purple")
                 (make-block BG-W 3 "purple")
                 (make-block BG-W 4 "purple"))
           empty) false)

(check-expect (rotate-cw? (make-posn 2 BG-L)
           (list (make-block 1 BG-L "purple")
                 (make-block 2 BG-L "purple")
                 (make-block 3 BG-L "purple")
                 (make-block 4 BG-L "purple"))
           empty) false)

(check-expect (rotate-cw? (make-posn 2 (- BG-L 2))
           (list (make-block 1 (- BG-L 2) "purple")
                 (make-block 2 (- BG-L 2) "purple")
                 (make-block 3 (- BG-L 2) "purple")
                 (make-block 4 (- BG-L 2) "purple"))
           (list (make-block 1 (- BG-L 1) "green")
                 (make-block 2 (- BG-L 1) "green")
                 (make-block 1 BG-L "green")
                 (make-block 2 BG-L "green"))) false)

(check-expect (rotate-cw? (make-posn 5 10)
           (list (make-block 4 10 "purple")
                 (make-block 5 10 "purple")
                 (make-block 6 10 "purple")
                 (make-block 7 10 "purple"))
           empty) true)

(define (rotate-cw? c abset1 abset2)
  (not (ormap (λ (x) (or (< (block-x (block-rotate-cw c x)) 1)
                    (> (block-x (block-rotate-cw c x)) BG-W)
                    (> (block-y (block-rotate-cw c x)) BG-L)
                    (block-contains? (block-rotate-cw c x) abset2))) abset1)))

;; rotate-ccw? Posn Bset Bset -> Boolean
;; Takes in two Bsets an checks if the first Bset is able to
;; rotate clockwise around the given Posn:
;; Conditions:
;; - rotation of Bset cannot put Bset out of the screen
;; - rotation of Bset cannot overlap another Bset

(check-expect (rotate-ccw? (make-posn 1 2)
           (list (make-block 1 1 "purple")
                 (make-block 1 2 "purple")
                 (make-block 1 3 "purple")
                 (make-block 1 4 "purple"))
           empty) false)

(check-expect (rotate-ccw? (make-posn BG-W 2)
           (list (make-block BG-W 1 "purple")
                 (make-block BG-W 2 "purple")
                 (make-block BG-W 3 "purple")
                 (make-block BG-W 4 "purple"))
           empty) false)

(check-expect (rotate-ccw? (make-posn 2 BG-L)
           (list (make-block 1 BG-L "purple")
                 (make-block 2 BG-L "purple")
                 (make-block 3 BG-L "purple")
                 (make-block 4 BG-L "purple"))
           empty) false)

(check-expect (rotate-ccw? (make-posn 2 (- BG-L 2))
           (list (make-block 1 (- BG-L 2) "purple")
                 (make-block 2 (- BG-L 2) "purple")
                 (make-block 3 (- BG-L 2) "purple")
                 (make-block 4 (- BG-L 2) "purple"))
           (list (make-block 2 (- BG-L 1) "green")
                 (make-block 3 (- BG-L 1) "green")
                 (make-block 2 BG-L "green")
                 (make-block 3 BG-L "green"))) false)

(check-expect (rotate-ccw? (make-posn 5 10)
           (list (make-block 4 10 "purple")
                 (make-block 5 10 "purple")
                 (make-block 6 10 "purple")
                 (make-block 7 10 "purple"))
           empty) true)

(define (rotate-ccw? c abset1 abset2)
  (not (ormap (λ (x) (or (< (block-x (block-rotate-ccw c x)) 1)
                    (> (block-x (block-rotate-ccw c x)) BG-W)
                    (> (block-y (block-rotate-ccw c x)) BG-L)
                    (block-contains? (block-rotate-ccw c x) abset2))) abset1)))


;; tetra-rotate-cw: Tetra -> Tetra
;; rotates a tetra 90 degrees clockwise around its center
(check-expect (tetra-rotate-cw (make-tetra
           (make-posn 4 3)
           (list (make-block 4 2 "aqua")
                 (make-block 4 3 "aqua")
                 (make-block 5 3 "aqua")
                 (make-block 6 3 "aqua"))))
              (make-tetra
               (make-posn 4 3)
               (list (make-block 3 3 "aqua")
                     (make-block 4 3 "aqua")
                     (make-block 4 2 "aqua")
                     (make-block 4 1 "aqua"))))

(define (tetra-rotate-cw atetra)
  (tetra-rotate-ccw (tetra-rotate-ccw (tetra-rotate-ccw atetra))))


;; combine: World -> Bset
;; combines the blocks from the Tetra in the world and the rest of
;; the blocks in the Block Set of the world into a single Bset

(check-expect (combine world1) (list (make-block 5 1 "green")
                 (make-block 6 1 "green")
                 (make-block 5 2 "green")
                 (make-block 6 2 "green")))

(check-expect (combine (make-world O (list (make-block 5 (- BG-L 1) "green")
                 (make-block 6 (- BG-L 1) "green")
                 (make-block 5 BG-L "green")
                 (make-block 6 BG-L "green")) 0))
              (list (make-block 5 1 "green")
                 (make-block 6 1 "green")
                 (make-block 5 2 "green")
                 (make-block 6 2 "green")
                 (make-block 5 (- BG-L 1) "green")
                 (make-block 6 (- BG-L 1) "green")
                 (make-block 5 BG-L "green")
                 (make-block 6 BG-L "green")))

(define (combine aworld)
  (append (tetra-blocks (world-tetra aworld))
          (world-pile aworld)))
                 

;;render: World -> Image
;; creates an image of the world

(check-expect
 (render world1)
 (place-image (overlay (square grid-scale "outline" "black")
            (square grid-scale "solid" "green")) 112.5 12.5
             (place-image (overlay (square grid-scale "outline" "black")
            (square grid-scale "solid" "green")) 137.5 12.5
                         (place-image (overlay
                                       (square grid-scale "outline" "black")
            (square grid-scale "solid" "green")) 112.5 37.5
                                     (place-image
                                      (overlay
                                       (square grid-scale "outline" "black")
            (square grid-scale "solid" "green"))
                                                 137.5 37.5 BG)))))

(define (render aworld)
        (foldr place-block BG (combine aworld)))

;; place-block: Block Image -> Image
;; places image of a block ontop of an image

(check-expect (place-block (make-block 5 1 "green") BG)
              (place-image (overlay (square grid-scale "outline" "black")
            (square grid-scale "solid" "green")) 112.5 12.5 BG))

(define (place-block ablock animage)
  (place-image
   (overlay (square grid-scale "outline" "black")
            (square grid-scale "solid" (block-color ablock)))
              (- (* (block-x ablock) grid-scale) (/ grid-scale 2))
              (- (* (block-y ablock) grid-scale) (/ grid-scale 2))
              animage))

;; bset-left : Bset -> Bset
;; Moves all blocks in a Bset to the left 1 grid unit

(check-expect (bset-left (list (make-block 3 1 "blue")
                 (make-block 4 1 "blue")
                 (make-block 5 1 "blue")
                 (make-block 6 1 "blue")))
              (list (make-block 2 1 "blue")
                    (make-block 3 1 "blue")
                    (make-block 4 1 "blue")
                    (make-block 5 1 "blue")))
(check-expect (bset-left (list (make-block 5 10 "blue")
                 (make-block 6 10 "blue")
                 (make-block 7 10 "blue")
                 (make-block 8 10 "blue")))
              (list (make-block 4 10 "blue")
                    (make-block 5 10 "blue")
                    (make-block 6 10 "blue")
                    (make-block 7 10 "blue")))

(define (bset-left abset)
  (map block-left abset))

;;block-left: Block -> Block
;; Shifts a Block 1 unit to the left
(check-expect (block-left (make-block 4 5 "blue")) (make-block 3 5 "blue"))

(define (block-left ablock)
  (make-block (- (block-x ablock) 1)
              (block-y ablock)
              (block-color ablock)))

;; tetra-left : Tetra -> Tetra
;; Shifts a given Tetra to the left 1 grid unit

(check-expect (tetra-left I)(make-tetra (make-posn 3 1)
                                        (list (make-block 2 1 "blue")
                                              (make-block 3 1 "blue")
                                              (make-block 4 1 "blue")
                                              (make-block 5 1 "blue"))))
(check-expect (tetra-left S)(make-tetra (make-posn 4 1)
                                        (list (make-block 5 1 "red")
                                              (make-block 4 1 "red")
                                              (make-block 3 2 "red")
                                              (make-block 4 2 "red"))))

(define (tetra-left atetra)
  (make-tetra (make-posn (- (posn-x (tetra-center atetra)) 1)
                         (posn-y (tetra-center atetra)))
              (bset-left (tetra-blocks atetra))))

;; left?: Bset Bset -> Boolean
;; Returns whether or not a Bset can shift to the left
;; Conditions:
;; - Bset cannot move off the screen
;; - Bset cannot overlap with another Bset

(check-expect (left? 
           (list (make-block 1 1 "purple")
                 (make-block 1 2 "purple")
                 (make-block 1 3 "purple")
                 (make-block 1 4 "purple"))
           empty) false)

(check-expect (left? 
           (list (make-block 4 BG-L "purple")
                 (make-block 5 BG-L "purple")
                 (make-block 6 BG-L "purple")
                 (make-block 7 BG-L"purple"))
           (list (make-block 2 (- BG-L 1) "green")
                 (make-block 3 (- BG-L 1) "green")
                 (make-block 2 BG-L "green")
                 (make-block 3 BG-L "green"))) false)

(check-expect (left? 
           (list (make-block 4 10 "purple")
                 (make-block 5 10 "purple")
                 (make-block 6 10 "purple")
                 (make-block 7 10 "purple"))
           empty) true)

(define (left? abset1 abset2)
  (not (ormap (λ (x) (or (< (block-x (block-left x)) 1)
                  (block-contains? (block-left x) abset2))) abset1)))


;; bset-right: Bset -> Bset
;; Moves all blocks in a Bset to the right

(check-expect (bset-right (list (make-block 3 1 "blue")
                 (make-block 4 1 "blue")
                 (make-block 5 1 "blue")
                 (make-block 6 1 "blue")))
              (list (make-block 4 1 "blue")
                    (make-block 5 1 "blue")
                    (make-block 6 1 "blue")
                    (make-block 7 1 "blue")))
(check-expect (bset-right (list (make-block 5 10 "blue")
                 (make-block 6 10 "blue")
                 (make-block 7 10 "blue")
                 (make-block 8 10 "blue")))
              (list (make-block 6 10 "blue")
                    (make-block 7 10 "blue")
                    (make-block 8 10 "blue")
                    (make-block 9 10 "blue")))

(define (bset-right abset)
  (map block-right abset))

;; block-right: Block -> Block
;; returns a block shifted over 1 unit

(check-expect (block-right (make-block 4 5 "blue")) (make-block 5 5 "blue"))

(define (block-right ablock)
  (make-block (+ 1 (block-x ablock))
              (block-y ablock)
              (block-color ablock)))

;; tetra-right : Tetra -> Tetra
;; Shifts a given Tetra to the right 1 grid unit

(check-expect (tetra-right I)(make-tetra (make-posn 5 1)
                                        (list (make-block 4 1 "blue")
                                              (make-block 5 1 "blue")
                                              (make-block 6 1 "blue")
                                              (make-block 7 1 "blue"))))
(check-expect (tetra-right S)(make-tetra (make-posn 6 1)
                                        (list (make-block 7 1 "red")
                                              (make-block 6 1 "red")
                                              (make-block 5 2 "red")
                                              (make-block 6 2 "red"))))

(define (tetra-right atetra)
  (make-tetra (make-posn (+ (posn-x (tetra-center atetra)) 1)
                         (posn-y (tetra-center atetra)))
              (bset-right (tetra-blocks atetra))))

;; right?: Bset Bset -> Boolean
;; Returns whether or not a Bset can shift to the right
;; Conditions:
;; - Bset cannot move off the screen
;; - Bset cannot overlap with another Bset

(check-expect (right? 
           (list (make-block BG-W 1 "purple")
                 (make-block BG-W 2 "purple")
                 (make-block BG-W 3 "purple")
                 (make-block BG-W 4 "purple"))
           empty) false)

(check-expect (right? 
           (list (make-block 4 BG-L "purple")
                 (make-block 5 BG-L "purple")
                 (make-block 6 BG-L "purple")
                 (make-block 7 BG-L"purple"))
           (list (make-block 8 (- BG-L 1) "green")
                 (make-block 9 (- BG-L 1) "green")
                 (make-block 8 BG-L "green")
                 (make-block 9 BG-L "green"))) false)

(check-expect (right? 
           (list (make-block 4 10 "purple")
                 (make-block 5 10 "purple")
                 (make-block 6 10 "purple")
                 (make-block 7 10 "purple"))
           empty) true)

(define (right? abset1 abset2)
  (not (ormap (λ (x) (or (> (block-x (block-right x)) BG-W)
                  (block-contains? (block-right x) abset2))) abset1)))

;; bset-down: Bset -> Bset
;; Moves all blocks in a Bset to down 1 unit

(check-expect (bset-down (list (make-block 3 1 "blue")
                 (make-block 4 1 "blue")
                 (make-block 5 1 "blue")
                 (make-block 6 1 "blue")))
              (list (make-block 3 2 "blue")
                    (make-block 4 2 "blue")
                    (make-block 5 2 "blue")
                    (make-block 6 2 "blue")))

(define (bset-down abset)
  (map block-down abset))

;; block-down: Block -> Block
;; Returns a block shifted down 1 unit

(check-expect (block-down (make-block 3 4 "blue")) (make-block 3 5 "blue"))

(define (block-down ablock)
  (make-block
   (block-x ablock)
   (+ (block-y ablock) 1)
   (block-color ablock)))

;; tetra-down : Tetra -> Tetra
;; Shifts a given Tetra to the right 1 grid unit

(check-expect (tetra-down I)(make-tetra (make-posn 4 2)
                                        (list (make-block 3 2 "blue")
                                              (make-block 4 2 "blue")
                                              (make-block 5 2 "blue")
                                              (make-block 6 2 "blue"))))
(check-expect (tetra-down S)(make-tetra (make-posn 5 2)
                                        (list (make-block 6 2 "red")
                                              (make-block 5 2 "red")
                                              (make-block 4 3 "red")
                                              (make-block 5 3 "red"))))

(define (tetra-down atetra)
  (make-tetra (make-posn (posn-x (tetra-center atetra))
                         (+ (posn-y (tetra-center atetra)) 1))
              (bset-down (tetra-blocks atetra))))

;; down?: Bset Bset -> Boolean
;; determines if a Bset can shift down 1 unit
;; Conditions:
;; - Blocks in Bset cannot fall below the screen
;; - Blocks in Bset cannot overlap another Bset

(check-expect (down? (list (make-block 3 BG-L "blue")) empty) false)

(check-expect (down? (list (make-block 3 (- BG-L 2) "blue"))
                      (list (make-block 2 (- BG-L 1) "green")
                            (make-block 3 (- BG-L 1) "green")
                            (make-block 2 BG-L "green")
                            (make-block 3 BG-L "green"))) false)


(check-expect (down? (list (make-block 10 10 "blue")) empty) true)

(define (down? abset1 abset2)
  (not (ormap (λ (x) (or (> (block-y (block-down x)) BG-L)
                         (block-contains? (block-down x) abset2))) abset1)))

;; tetra-drop: World -> World
;; drops the tetra all the way down to the lowest possible place.

(check-expect (tetra-drop world1) (make-world
                                   (make-tetra
                                    (make-posn 6 (- BG-L 1))
                                    (list (make-block 5 (- BG-L 1) "green")
                                          (make-block 6 (- BG-L 1) "green")
                                          (make-block 5 BG-L "green")
                                          (make-block 6 BG-L "green")))
                                   empty 0))

(check-expect (tetra-drop world2) (make-world
                                    (make-tetra
                                     (make-posn 6 (- BG-L 2))
                                     (list (make-block 6 (- BG-L 3) "purple")
                                           (make-block 4 (- BG-L 2) "purple")
                                           (make-block 5 (- BG-L 2) "purple")
                                           (make-block 6 (- BG-L 2) "purple")))
                                   (list (make-block 5 BG-L "green")
                                         (make-block 6 BG-L "green")
                                         (make-block 5 (- BG-L 1) "green")
                                         (make-block 6 (- BG-L 1) "green")) 0))

(define (tetra-drop aworld)
  (cond [(down? (tetra-blocks (world-tetra aworld)) (world-pile aworld))
         (tetra-drop (make-world (tetra-down (world-tetra aworld))
                                 (world-pile aworld) (world-score aworld)))]
        [else aworld]))

;; blocks-in-line: Number Bset -> Bset
;; Returns a list of all blocks that exists on a given y-coordinate on the
;; grid

(check-expect (blocks-in-line 5 empty) empty)

(check-expect (blocks-in-line BG-L (list (make-block 3 BG-L "green")
                                       (make-block 4 BG-L "green")
                                       (make-block 5 BG-L "green")
                                       (make-block 3 4 "green")
                                       (make-block 4 4 "green")
                                       (make-block 5 4 "green")))
              (list (make-block 3 BG-L "green")
                    (make-block 4 BG-L "green")
                    (make-block 5 BG-L "green")))

(define (blocks-in-line anum abset)
  (filter (λ (x) (= anum (block-y x))) abset))

;; line?: Number Bset -> Boolean
;; returns true if blocks exist in all x-coordinates for a given y-coordinate
;; on the grid

(check-expect (line? 3 (list (make-block 1 3 "blue")
                             (make-block 2 3 "blue")
                             (make-block 3 3 "blue")
                             (make-block 4 3 "blue")
                             (make-block 5 3 "blue")
                             (make-block 6 3 "blue")
                             (make-block 7 3 "blue")
                             (make-block 8 3 "blue")
                             (make-block 9 3 "blue")
                             (make-block 10 3 "blue"))) true)

(check-expect (line? 3 (list (make-block 1 3 "blue")
                             (make-block 2 3 "blue")
                             (make-block 3 3 "blue")
                             (make-block 4 3 "blue")
                             (make-block 5 3 "blue")
                             (make-block 6 3 "blue"))) false)

(check-expect (line? 3 empty) false)

(define (line? anum abset)
  (= (length (blocks-in-line anum abset)) BG-W))

;; tetra-y: Tetra -> [List-of Number]
;; returns a list of the y coordinates of the blocks in a tetra

(check-expect (tetra-y O) (list 1 1 2 2))

(define (tetra-y atetra)
  (map block-y (tetra-blocks atetra)))

;; Tetra Bset -> Boolean
;; returns true if there are any completed lines with the y values of any block
;; in a given tetra

#; (check-expect (tetra-line? (list (make-block 1 3 "blue")
                             (make-block 2 3 "blue")
                             (make-block 3 3 "blue")
                             (make-block 4 3 "blue")
                             (make-block 5 3 "blue")
                             (make-block 6 3 "blue")
                             (make-block 7 3 "blue")
                             (make-block 8 3 "blue")
                             (make-block 9 3 "blue"))))


;; remove-line: Bset Number -> Bset 
;; removes all blocks in a given row and shifts blocks above down

(check-expect (remove-line (list (make-block 1 BG-L "blue")
                             (make-block 2 BG-L "blue")
                             (make-block 3 BG-L "blue")
                             (make-block 4 BG-L "blue")
                             (make-block 5 BG-L "blue")
                             (make-block 6 BG-L "blue")
                             (make-block 7 BG-L "blue")
                             (make-block 8 BG-L "blue")
                             (make-block 9 BG-L "blue")
                             (make-block 10 BG-L "blue")
                             (make-block 10 (- BG-L 1) "blue")) BG-L)
                          (list (make-block 10 BG-L "blue")) )

(check-expect (remove-line empty BG-L) empty)

(check-expect (remove-line (list (make-block 1 BG-L "blue")
                             (make-block 2 BG-L "blue")
                             (make-block 3 BG-L "blue")
                             (make-block 4 BG-L "blue")
                             (make-block 5 BG-L "blue")
                             (make-block 6 BG-L "blue")
                             (make-block 7 BG-L "blue")
                             (make-block 8 BG-L "blue")
                             (make-block 9 BG-L "blue")) BG-L)
              empty)

(check-expect (remove-line (list (make-block 1 (- BG-L 1) "blue")
                             (make-block 1 BG-L "blue")) (- BG-L 1))
              (list(make-block 1 BG-L "blue")))

                          
            
(define (remove-line abset anum)
  (foldr (λ (x y) (cond
                    [(= (block-y x) anum) y]
                    [(< (block-y x) anum)
                     (cons (block-down x) y)]
                    [(> (block-y x) anum)
                     (cons x y)])) empty abset))

;; remove-all-lines: Bset -> Bset

(check-expect (remove-all-lines (list (make-block 1 BG-L "blue")
                             (make-block 2 BG-L "blue")
                             (make-block 3 BG-L "blue")
                             (make-block 4 BG-L "blue")
                             (make-block 5 BG-L "blue")
                             (make-block 6 BG-L "blue")
                             (make-block 7 BG-L "blue")
                             (make-block 8 BG-L "blue")
                             (make-block 9 BG-L "blue")
                             (make-block 10 BG-L "blue")
                             (make-block 8 (- BG-L 1) "blue")
                             (make-block 9 (- BG-L 1) "blue")
                             (make-block 7 (- BG-L 1) "blue")
                             (make-block 6 (- BG-L 1) "blue")
                             (make-block 5 (- BG-L 1) "blue")) BG-L)
              (list (make-block 8 BG-L "blue")
                             (make-block 9 BG-L "blue")
                             (make-block 7 BG-L "blue")
                             (make-block 6 BG-L "blue")
                             (make-block 5 BG-L "blue")))

(check-expect (remove-all-lines empty BG-L) empty)

(define (remove-all-lines abset anum)
 (cond [(line? anum abset) (remove-all-lines (remove-line abset anum) (add1 anum))]
       [(<= anum BG-L) (remove-all-lines abset (add1 anum))]
       [else abset]))
       

;; tock: World -> World
;; returns a World with the Tetra of the World moved down 1 grid-unit

(define (tock aworld)
  (cond
    [(down? (tetra-blocks (world-tetra aworld)) (world-pile aworld))
      (make-world (tetra-down (world-tetra aworld))
                  (world-pile aworld)
                  (add1 (world-score aworld)))]
     [else (make-world (list-ref Tset (random 7))
                       (remove-all-lines (combine aworld) 1)
                       (+ 8 (world-score aworld)))]))

;; control-tetra: World KE -> World
;; Takes in a key event and changes the falling Tetra accordingly.
;; Takes in one of:
;; "left" (shifts Tetra to the left)
;; "right" (shifts Tetra to the right)
;; "down" (shifts Tetra down one grid unit)
;; " " (drops the Tetra down to the lowest possible unoccupied grid unit)
;; "s" (rotates the Tetra 90 degrees clockwise)
;; "a" (rotates the Tetra 90 degrees counterclockwise)

(define (control-tetra aworld ke)
  (cond
    [(and (key=? ke "down")
          (down? (tetra-blocks (world-tetra aworld)) (world-pile aworld)))
     (make-world (tetra-down (world-tetra aworld)) (world-pile aworld)
                 (world-score aworld))]
    [(and (key=? ke "left")
          (left? (tetra-blocks (world-tetra aworld)) (world-pile aworld)))
     (make-world (tetra-left (world-tetra aworld)) (world-pile aworld)
                 (world-score aworld))]
    [(and (key=? ke "right")
          (right? (tetra-blocks (world-tetra aworld)) (world-pile aworld)))
     (make-world (tetra-right (world-tetra aworld)) (world-pile aworld)
                 (world-score aworld))]
    [(and (key=? ke "s")
          (rotate-cw? (tetra-center (world-tetra aworld))
                      (tetra-blocks (world-tetra aworld)) (world-pile aworld)))
     (make-world (tetra-rotate-cw (world-tetra aworld)) (world-pile aworld)
                 (world-score aworld))]
    ;; makes rotate-cw more smooth by allowing rotations against the right edge
    ;; and allowing rotatations against the right side of another tetra
    ;; if a block is against a right edge, it shifts it to the left and rotates
    ;; if and only if the original tetra is able to shift to the left
    [(and (key=? ke "s") 
          (rotate-cw? (tetra-center (tetra-left (world-tetra aworld)))
                      (tetra-blocks (tetra-left (world-tetra aworld)))
                      (world-pile aworld))
          (left? (tetra-blocks (world-tetra aworld)) (world-pile aworld)))
     (make-world (tetra-rotate-cw (tetra-left (world-tetra aworld)))
                 (world-pile aworld)
                 (world-score aworld))]
    ;; makes rotate-cw more smooth by allowing rotations against the left edge
    ;; and allowing rotatations against the left side of another tetra
    ;; if a block is against a left edge, it shifts it to the right and rotates
    ;; if and only if the original tetra is able to shift to the right
    [(and (key=? ke "s")
          (rotate-cw? (tetra-center (tetra-right (world-tetra aworld)))
                      (tetra-blocks (tetra-right (world-tetra aworld)))
                      (world-pile aworld))
          (right? (tetra-blocks (world-tetra aworld)) (world-pile aworld)))
     (make-world (tetra-rotate-cw (tetra-right (world-tetra aworld)))
                 (world-pile aworld)
                 (world-score aworld))]
    [(and (key=? ke "a")
          (rotate-ccw? (tetra-center (world-tetra aworld))
                      (tetra-blocks (world-tetra aworld)) (world-pile aworld)))
     (make-world (tetra-rotate-ccw (world-tetra aworld)) (world-pile aworld)
                 (world-score aworld))]
    ;; makes rotate-ccw more smooth by allowing rotations against the right edge
    ;; and allowing rotate-ccw against the right side of another tetra
    ;; if a block is against a right edge, it shifts it to the left and rotates
    ;; if and only if the original tetra is able to shift to the left
    [(and (key=? ke "a")
          (rotate-ccw? (tetra-center (tetra-left (world-tetra aworld)))
                      (tetra-blocks (tetra-left (world-tetra aworld)))
                      (world-pile aworld))
          (left? (tetra-blocks (world-tetra aworld)) (world-pile aworld)))
     (make-world (tetra-rotate-ccw (tetra-left (world-tetra aworld)))
                 (world-pile aworld)
                 (world-score aworld))]
    ;; makes rotate-ccw more smooth by allowing rotations against the left edge
    ;; and allowing rotate-ccw against the left side of another tetra
    ;; if a block is against a left edge, it shifts it to the right and rotates
    ;; if and only if the original tetra is able to shift to the right
    [(and (key=? ke "a")
          (rotate-ccw? (tetra-center (tetra-right (world-tetra aworld)))
                      (tetra-blocks (tetra-right (world-tetra aworld)))
                      (world-pile aworld))
          (right? (tetra-blocks (world-tetra aworld)) (world-pile aworld)))
     (make-world (tetra-rotate-ccw (tetra-right (world-tetra aworld)))
                 (world-pile aworld)
                 (world-score aworld))]
    [(key=? ke " ")
     (tetra-drop aworld)]
    [else aworld]))

;; lose: WS -> Boolean
;; returns true when the player loses the game
;; interp: The player loses the game when the blocks
;; reach the top of the game screen. 
;; (The fuction works by determining if a new tetra spawned on top of
;; the block located at the center top. When this happens, the player loses)

(define (lose aworld)
  (bset-contains? (tetra-blocks
                   (world-tetra aworld))
                  (world-pile aworld)))

;;score-page: WS -> Image
;; Image produced after the big-bang function ends

(check-expect (score-page (make-world O empty 0))
              (place-image (text "Game Over" 30 "red")
                      (/ (* grid-scale BG-W) 2)
                      (/ (* grid-scale BG-L) 3)
                      (place-image
                       (text
                        "Score: 0" 30 "red")
                       (/ (* grid-scale BG-W) 2)
                       (/ (* grid-scale BG-L) 2)
                        (empty-scene (* grid-scale BG-W)
                                     (* grid-scale BG-L)))))

(define (score-page aworld)
  (place-image (text "Game Over" 30 "red")
                      (/ (* grid-scale BG-W) 2)
                      (/ (* grid-scale BG-L) 3)
                      (place-image
                       (text
                        (string-append "Score: "
                                       (number->string
                                        (world-score aworld)))
                       30 "red")
                       (/ (* grid-scale BG-W) 2)
                       (/ (* grid-scale BG-L) 2)
                        (empty-scene (* grid-scale BG-W)
                                     (* grid-scale BG-L)))))

(define (main ws)
  (big-bang ws
            [on-tick tock 0.25]
            [to-draw render]
            [on-key control-tetra]
            [stop-when lose score-page]))

                                                                  
(main (make-world (list-ref Tset (random 7)) empty 0))              