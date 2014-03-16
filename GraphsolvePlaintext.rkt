;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname GraphsolvePlaintext) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require racket/gui/base)
;; -----STRUCTURES-----

(define-struct tile (top rig bot lef num))
;; a top/lef/bot/rig is a list containing both one of
;;   *'green
;;   *'blue
;;   *'red
;;   *'yellow
;; and one of
;;   *'head
;;   *'tail
;; a num is an arbitrary unique tile identifier, a number
;;  between 1 and 9

(define-struct node (posn tile subtree))
;; a posn is one of the nine positions in a 3x3 grid
;;  according to the following diagram:
;;    6 2 7
;;    5 1 3
;;    9 4 8
;; a tile is a tile
;; a subtree is a list of nodes containing possible
;;  adjascent tiles

;; a tree is a list of nodes with no incoming nodes

;; EX of a tree, note: (= node-posn 6) can not be a part of
;;  (= node-posn 1) subtree (by definition)
; (make-node 1 tile1 '((make-node 2 ...)
;                      (make-node 2 ...)
;                      (make-node 3 ...)
;                      (make-node 4 ...)
;                      (make-node 4 ...)
;                      (make-node 5 ...)))


;; -----DATA DEFINITIONS-----

(define tile1 (make-tile (list 'green 'head) (list 'red 'tail) (list 'yellow 'head) (list 'blue 'head) 1))
(define tile2 (make-tile (list 'blue 'head) (list 'green 'head) (list 'red 'head) (list 'yellow 'head) 2))
(define tile3 (make-tile (list 'blue 'tail) (list 'yellow 'head) (list 'red 'head) (list 'green 'head) 3))
(define tile4 (make-tile (list 'red 'head) (list 'green 'head) (list 'yellow 'tail) (list 'blue 'head) 4))
(define tile5 (make-tile (list 'yellow 'tail) (list 'blue 'tail) (list 'green 'head) (list 'red 'head) 5))
(define tile6 (make-tile (list 'yellow 'head) (list 'green 'tail) (list 'red 'head) (list 'blue 'head) 6))
(define tile7 (make-tile (list 'red 'head) (list 'green 'tail) (list 'blue 'head) (list 'yellow 'head) 7))
(define tile8 (make-tile (list 'green 'tail) (list 'yellow 'head) (list 'blue 'head) (list 'red 'tail) 8))
(define tile9 (make-tile (list 'red 'tail) (list 'blue 'tail) (list 'green 'head) (list 'yellow 'tail) 9))
(define tilelist (list tile1 tile2 tile3 tile4 tile5 tile6 tile7 tile8 tile9))

;; -----DEFINITIONS-----

;; anti takes a tile edge and returns the same list but with 'head or 'tail switched
(define (anti edge)
  (cond [(symbol=? (second edge) 'head)
         (list (first edge) 'tail)]
        [(symbol=? (second edge) 'tail)
         (list (first edge) 'head)]))

;; make-tree consumes a tlist and an empty tree and produces the first layer of nodes in the tree, then
;;  calls make-tree-acc to build the rest of the tree
(define (make-tree tlist tree)
  (map (λ (tile1) (make-node 1 tile1 ((λ (tl) (make-edge-nodes (list (filter (λ (x) (cond [(boolean? x) false]
                                                                                          [else true]))
                                                                             (map (λ (tile) (orient-edge 'top (anti (tile-top tile1)) tile 0)) tlist))
                                                                     (filter (λ (x) (cond [(boolean? x) false]
                                                                                          [else true]))
                                                                             (map (λ (tile) (orient-edge 'rig (anti (tile-rig tile1)) tile 0)) tlist))
                                                                     (filter (λ (x) (cond [(boolean? x) false]
                                                                                          [else true]))
                                                                             (map (λ (tile) (orient-edge 'bot (anti (tile-bot tile1)) tile 0)) tlist))
                                                                     (filter (λ (x) (cond [(boolean? x) false]
                                                                                          [else true]))
                                                                             (map (λ (tile) (orient-edge 'lef (anti (tile-lef tile1)) tile 0)) tlist)))
                                                               tile1 tlist)) tlist)))
       tlist))


;; orient-edges consumes *A position to check
;;                       *The edge to be matched with
;;                       *The tilelist
;; Returns a properly oriented, compatable tile, or false
(define (orient-edge edgesym antiedge tile rot-acc)
  (cond [(symbol=? edgesym 'top) (cond [(= rot-acc 4) false]
                                       [(equal? antiedge (tile-bot tile)) tile]
                                       [else (orient-edge edgesym antiedge (rot tile) (add1 rot-acc))])]
        [(symbol=? edgesym 'rig) (cond [(= rot-acc 4) false]
                                       [(equal? antiedge (tile-lef tile)) tile]
                                       [else (orient-edge edgesym antiedge (rot tile) (add1 rot-acc))])]
        [(symbol=? edgesym 'bot) (cond [(= rot-acc 4) false]
                                       [(equal? antiedge (tile-top tile)) tile]
                                       [else (orient-edge edgesym antiedge (rot tile) (add1 rot-acc))])]
        [(symbol=? edgesym 'lef) (cond [(= rot-acc 4) false]
                                       [(equal? antiedge (tile-rig tile)) tile]
                                       [else (orient-edge edgesym antiedge (rot tile) (add1 rot-acc))])]))

;; rot rotates a tile
(define (rot tile)
  (make-tile (tile-lef tile) (tile-top tile) (tile-rig tile) (tile-bot tile) (tile-num tile)))

;; make-edge-nodes consumes *a list of 4 lists, pertaining to tiles that match the 4 edges of the middle tile
;;                          *The tile in position 1 for this branch of the tree
;;                          *The tilelist
;; Returns a list of nodes
(define (make-edge-nodes tlists tile1 tlist)
  (map (λ (tile2) (make-node 2 tile2 (map (λ (tile3) (make-node 3 tile3 (map (λ (tile4) (make-node 4 tile4 (map (λ (tile5) (make-node 5 tile5 (make-corner-nodes tlist 6 (list tile1
                                                                                                                                                                               tile2
                                                                                                                                                                               tile3
                                                                                                                                                                               tile4
                                                                                                                                                                               tile5)
                                                                                                                                                                 (filter (λ (tile) (cond [(member? tile (list tile1
                                                                                                                                                                                                              tile2
                                                                                                                                                                                                              tile3
                                                                                                                                                                                                              tile4
                                                                                                                                                                                                              tile5)) false]
                                                                                                                                                                                         [else true])) tlist))))
                                                                                                                (fourth tlists))))
                                                                             (third tlists))))
                                          (second tlists))))
       (first tlists)))

;; check-corner checks if a corner matches
;; Returns bool
(define (check-corner posn tile antiedge1 antiedge2 rot-acc)
  (cond [(= posn 6) (cond [(= rot-acc 4) false]
                          [(equal? antiedge1 (tile-lef tile)) (cond [(equal? antiedge2 (tile-bot tile)) true]
                                                                    [else false])]
                          [else (check-corner posn (rot tile) antiedge1 antiedge2 (add1 rot-acc))])]
        [(= posn 7) (cond [(= rot-acc 4) false]
                          [(equal? antiedge1 (tile-top tile)) (cond [(equal? antiedge2 (tile-lef tile)) true]
                                                                    [else false])]
                          [else (check-corner posn (rot tile) antiedge1 antiedge2 (add1 rot-acc))])]
        [(= posn 8) (cond [(= rot-acc 4) false]
                          [(equal? antiedge1 (tile-rig tile)) (cond [(equal? antiedge2 (tile-top tile)) true]
                                                                    [else false])]
                          [else (check-corner posn (rot tile) antiedge1 antiedge2 (add1 rot-acc))])]
        [else       (cond [(= rot-acc 4) false]
                          [(equal? antiedge1 (tile-bot tile)) (cond [(equal? antiedge2 (tile-rig tile)) true]
                                                                    [else false])]
                          [else (check-corner posn (rot tile) antiedge1 antiedge2 (add1 rot-acc))])]))

;; check-corner-wrap
(define (check-corner-wrap posn tile atile1 atile2)
  (cond [(= posn 6) (check-corner posn tile (anti (tile-rig atile1)) (anti (tile-top atile2)) 0)]
        [(= posn 7) (check-corner posn tile (anti (tile-bot atile1)) (anti (tile-rig atile2)) 0)]
        [(= posn 8) (check-corner posn tile (anti (tile-lef atile1)) (anti (tile-bot atile2)) 0)]
        [else       (check-corner posn tile (anti (tile-top atile1)) (anti (tile-lef atile2)) 0)]))

;; make-corner-nodes consumes *The tilelist
;;                            *The tile position being listed
;;                            *The list of used tiles
;;                            *The list of unused tiles
;; returns a list of nodes
(define (make-corner-nodes tlist tile-posn used unused)
  (cond [(= tile-posn 6) (map (λ (tile6) (make-node 6 tile6 (make-corner-nodes tlist 7 (append used (list tile6)) (filter (λ (x) (cond [(equal? x tile6) false]
                                                                                                                                       [else true]))
                                                                                                                          unused))))
                              (filter (λ (tile) (check-corner-wrap 6 tile (second used) (third used))) unused))]
        [(= tile-posn 7) (map (λ (tile7) (make-node 7 tile7 (make-corner-nodes tlist 8 (append used (list tile7)) (filter (λ (x) (cond [(equal? x tile7) false]
                                                                                                                                       [else true]))
                                                                                                                          unused))))
                              (filter (λ (tile) (check-corner-wrap 7 tile (third used) (fourth used))) unused))]
        [(= tile-posn 8) (map (λ (tile8) (make-node 8 tile8 (make-corner-nodes tlist 9 (append used (list tile8)) (filter (λ (x) (cond [(equal? x tile8) false]
                                                                                                                                       [else true]))
                                                                                                                          unused))))
                              (filter (λ (tile) (check-corner-wrap 8 tile (fourth used) (fifth used))) unused))]
        [else (map (λ (tile9) (make-node 9 tile9 '()))
                   (filter (λ (tile) (check-corner-wrap 9 tile (fifth used) (second used))) unused))]))

;; ----- FIND SOLUTION -----
;; display-soln finds and outputs the solution to the puzzle
(define (display-soln tree)
  (cond [(empty? tree) false]
        [(empty? (node-subtree (first tree)))
         (cond [(= (node-posn (first tree)) 9) (cons (tile-num (node-tile (first tree))) '())]
               [else false])]
        [(let [(soln-sb-fst (display-soln (node-subtree (first tree))))]
           (and (not ((λ (z) (cond [(cons? z) (member? (tile-num (node-tile (first tree))) z)]
                                   [else false])) soln-sb-fst))
                (cons? soln-sb-fst)))
         (cons (tile-num (node-tile (first tree)))
               (display-soln (node-subtree (first tree))))]
        [else (display-soln (rest tree))]))

;; ----- UI -----
(begin (display "Enter any letter to begin solving the puzzle :") (newline)
       (read)
       (let [(time1 (current-milliseconds))]
         (begin (display "Bulding Directed Graph...") (newline)
                (let [(tree (make-tree tilelist empty))]
                  (begin (display "Graph Built!") (newline)
                         (display "Searching Graph for Solution...") (newline)
                         (let [(soln (display-soln tree))]
                           (cond [(boolean? soln) (display "No solution found.")]
                                 [else (begin (display "Solution Found!") (newline) (newline)
                                              (display #\space) (display #\space) (display #\space)
                                              (display #\space) (display #\space)
                                              (display (first (rest (rest (rest (rest (rest (rest (rest (rest soln)))))))))) (display #\space)
                                              (display (second soln)) (display #\space)
                                              (display (sixth soln)) (newline)
                                              (display #\space) (display #\space) (display #\space)
                                              (display #\space) (display #\space)
                                              (display (fifth soln)) (display #\space)
                                              (display (first soln)) (display #\space)
                                              (display (third soln)) (newline)
                                              (display #\space) (display #\space) (display #\space)
                                              (display #\space) (display #\space)
                                              (display (eighth soln)) (display #\space)
                                              (display (fourth soln)) (display #\space)
                                              (display (seventh soln)) (newline) (newline)
                                              (display "Time to completion : ")
                                              (display (- (current-milliseconds) time1))
                                              (display " milliseconds"))])))))))