(ns clojure-a2.core
  (:gen-class)
  (:require [cheshire.core :refer [parse-string generate-string]]))

;; find all possible moves using list comprehension
;; @param startX: starting x position
;; @param startY: starting y position
;; @param board: 2d array with height of spaces
;; @param teammate: location of teammate
;; @param otherPlayer: array with the location of the other two players
(defn possibleMoves [startX startY board teammate otherPlayer]

  (let [currentLevel (get (get board (- startX 1)) (- startY 1))
        availMoves
        (for [x (range (- startX 1) (+ 2 startX) 1)
              y (range (- startY 1) (+ 2 startY) 1)
              :when (and (> x 0) (> y 0) (< x 6) (< y 6)
                         (or (not= x startX) (not= y startY))
                         (or (not= x (get (get otherPlayer 0) 0)) (not= y (get (get otherPlayer 0) 1)))
                         (or (not= x (get teammate 0)) (not= y (get teammate 1)))
                         (or (not= x (get (get otherPlayer 1) 0)) (not= y (get (get otherPlayer 1) 1)))
                         (<= (get (get board (- x 1)) (- y 1)) (+ 1 currentLevel)))]
          [x y])]
    availMoves))


;; find all possible builds 
;; @param startX: starting x position
;; @param startY: starting y position
;; @param board: 2d array with height of spaces
;; @param teammate: location of teammate
;; @param otherPlayer: array with the location of the other two players
(defn possibleBuilds [startX startY board teammate otherPlayer]
  
  (let [currentLevel (get (get board (- startX 1)) (- startY 1))
        availMoves
        (for [x (range (- startX 1) (+ 2 startX) 1)
              y (range (- startY 1) (+ 2 startY) 1)
              :when (and (> x 0) (> y 0) (< x 6) (< y 6)
                         (or (not= x startX) (not= y startY))
                         (or (not= x (get (get otherPlayer 0) 0)) (not= y (get (get otherPlayer 0) 1)))
                         (or (not= x (get teammate 0)) (not= y (get teammate 1)))
                         (or (not= x (get (get otherPlayer 1) 0)) (not= y (get (get otherPlayer 1) 1)))
                         (< (get (get board (- x 1)) (- y 1)) 4))]
          [x y])]
    availMoves))

;; check if any of the possible moves result in a win 
;; @param board: 2d array with height of spaces
;; @param newPositions: array of possible moves
(defn isGameWon [board newPositions pan oldPositionHeight]
  (if (not= (count newPositions) 0)
    (loop [i 0]
      (if (or (= 3 (get-in board [(- (get-in newPositions [i 0]) 1) (- (get-in newPositions [i 1]) 1)])) 
              (and 
               pan 
               (<= 2 (- oldPositionHeight (get-in board [(- (get-in newPositions [i 0]) 1) (- (get-in newPositions [i 1]) 1)])))))
        (get newPositions i)
        (if (= (+ i 1) (count newPositions))
          false
          (recur (+ i 1))))
      )
    false
    )
  )


;; check if any of the possible moves result in a loss (this is identical to the above function, I should just alias it to a new name for confusion.) 
;; @param board: 2d array with height of spaces
;; @param newPositions: array of possible moves
(defn isGameLost [board newPositions]
  (if (not= (count newPositions) 0)
    (loop [i 0]
      (if (= 3 (get-in board [(- (get-in newPositions [i 0]) 1) (- (get-in newPositions [i 1]) 1)]))
        (get newPositions i)
        (if (= (+ i 1) (count newPositions))
          false
          (recur (+ i 1)))))
    false)
  )


;; create the outputted json
;; @param playerIndex: which piece moved
;; @param move: array representing where the player moved
;; @param build: array representing where the player built, false of no build
;; @param parsed: original json
(defn createJson [playerIndex move build parsed]
  (let [newParsed (assoc parsed
                         "turn"
                         (+ 1 (get parsed "turn")))
        newPlayers (assoc (get (get parsed "players") 0) "tokens" (assoc (get (get (get parsed "players") 0) "tokens")
                          playerIndex
                          move))
        newParsed (assoc newParsed
                         "players"
                         [(get (get newParsed "players") 1) newPlayers])
        newSpaces (if build
                    (assoc (get newParsed "spaces")
                           (- (get build 0) 1)
                           (assoc
                            (get (get newParsed "spaces") (- (get build 0) 1)) (- (get build 1) 1) (+ 
                                                                                                    (get-in (get newParsed "spaces") [(- (get build 0) 1) (- (get build 1) 1)])
                                                                                                    1)))
                    (get newParsed "spaces"))
        newParsed (assoc newParsed
                         "spaces"
                         newSpaces)]
    newParsed))

;; Checks if a piece can build at a specific spot
;; @param spaceX: x value to check if we can build at
;; @param spaceX: y value to check if we can build at
;; @param newPositionsPlayer: array of possible moves for one piece
;; @param teammateIndex: index for my teammate (0 or 1)
;; @param parsed: original json
(defn buildAtSpace [spaceX spaceY newPositionsPlayer teammateIndex parsed]
  (if (not= (count newPositionsPlayer) 0)
    (loop [i 0]

      (let [moves (possibleBuilds
                   (get-in newPositionsPlayer [i 0])
                   (get-in newPositionsPlayer [i 1])
                   (get parsed "spaces")
                   (get-in (get parsed "players") [0 "tokens" teammateIndex])
                   (get-in (get parsed "players") [1 "tokens"]))]

        (if (< -1 (.indexOf moves [spaceX spaceY]))
          (vec [(get newPositionsPlayer i) [spaceX spaceY]])
          (if (= (+ i 1) (count newPositionsPlayer))
            false
            (recur (+ i 1))))))
    false
    )
)



;; Find the best move for a given piece. awful logic rn. 
;; @param board: 2d array with height of spaces
;; @param newPositionsPlayer: array of possible moves   
;; @param teammateIndex: index for my teammate (0 or 1)
;; @param parsed: original json
;; @param minScore: minimum accepted score 


(defn getFirstCard [parsed]
  (get (get (get parsed "players") 0) "card"))

(defn getFirstTokens [parsed]
  (get (get (get parsed "players") 0) "tokens"))

(defn getZeroZeroToken [parsed]
  (get (get (get (get parsed "players") 0) "tokens") 0))

(defn getZeroOneToken [parsed]
  (get (get (get (get parsed "players") 0) "tokens") 1))

(defn getOneZeroToken [parsed]
  (get (get (get (get parsed "players") 1) "tokens") 0))

(defn getOneOneToken [parsed]
  (get (get (get (get parsed "players") 1) "tokens") 1))

(defn getSecondCard [parsed]
  (get (get (get parsed "players") 1) "card"))

(defn getSecondTokens [parsed]
  (get (get (get parsed "players") 1) "tokens"))

(defn getHeightAt [board x y]
  (get-in board [(- x 1) (- y 1)])
  )

;; Gives a score for a given move based on, currently nothing. 
;; @param move: array representing location moved to 
;; @param build: array represnting location built at
;; @param parsed: original json
(defn scoreMove [move build parsed]
  ; moving up is good 
  ; building to your own height += 1 is good 
  (let [moveHeight (getHeightAt (get parsed "spaces") (get move 0) (get move 1))
        buildHeight (getHeightAt (get parsed "spaces") (get build 0) (get build 1))
        heightUp (for [x (range (- (get build 0) 1) (+ 2 (get build 0)) 1)
                       y (range (- (get build 1) 1) (+ 2 (get build 1)) 1)
                       :when (and (> x 0) (> y 0) (< x 6) (< y 6)
                                  (or (not= x (get build 0)) (not= y (get build 1)))
                                  (= (getHeightAt (get parsed "spaces") x y) (getHeightAt (get parsed "spaces") (get build 0) (get build 1)))
                                  (< (get (get (get parsed "spaces") (- x 1)) (- y 1)) 4))]
                   [x y])]
    (if (> (count heightUp) 0)
      (+ moveHeight buildHeight)
      moveHeight
      )
    ))

(defn makeMove [newPositionsPlayer teammateIndex parsed minScore]
  (if (not= (count newPositionsPlayer) 0)
    (loop [i 0]
      (let [moves (vec (possibleBuilds
                        (get-in newPositionsPlayer [i 0])
                        (get-in newPositionsPlayer [i 1])
                        (get parsed "spaces")
                        (get-in (get parsed "players") [0 "tokens" teammateIndex])
                        (get-in (get parsed "players") [1 "tokens"])))
            newMove
            (loop [j 0]
              (let [score (scoreMove (get newPositionsPlayer i) (get moves j) parsed)]
                (if (>= score minScore)
                  (vec [(get newPositionsPlayer i) (get moves j) score])
                  (if (= (+ j 1) (count moves))
                    false
                    (recur (+ j 1))))))]

        (if newMove
          newMove
          (if (= (+ i 1) (count newPositionsPlayer))
            (makeMove newPositionsPlayer teammateIndex parsed (- minScore 1))
            (recur (+ i 1))))))
    [-1 -1 -1]))

(defn setup [wholeJson]

  
  (let [playerArr (vec(parse-string wholeJson))
        fourCorners [[2, 2], [4, 4], [2, 4], [4, 2]]
        opponentTokens (get (get playerArr 1) "tokens")]
    
  (if opponentTokens
    ; if the first token works
    (if (= (.indexOf opponentTokens (get fourCorners 0)) -1)
      ; and the second token works
      (if (= (.indexOf opponentTokens (get fourCorners 1)) -1)
        [(get playerArr 1),  (assoc (get playerArr 0) "tokens" [[2, 2], [4, 4]])]
        ; second token fails third has to work
        (if (= (.indexOf opponentTokens (get fourCorners 2)) -1)
          [(get playerArr 1), (assoc (get playerArr 0) "tokens" [[2, 2], [2, 4]])]
          [(get playerArr 1), (assoc (get playerArr 0) "tokens" [[2, 2], [4, 2]])]))

      ; first token failed, second token works
      (if (= (.indexOf opponentTokens (get fourCorners 1)) -1)
        (if (= (.indexOf opponentTokens (get fourCorners 2)) -1)
          [(get playerArr 1), (assoc (get playerArr 0) "tokens" [[4, 4], [2, 4]])]
          [(get playerArr 1), (assoc (get playerArr 0) "tokens" [[4, 4], [4, 2]])])
        [(get playerArr 1),  (assoc (get playerArr 0) "tokens" [[2, 4], [4, 2]])])
        )
    [(get playerArr 1),  (assoc (get playerArr 0) "tokens" [[2, 2], [4, 4]])]
    )
      )
  
    )

(defn takeTurn [wholeJson]

  ;;first get the possible moves for both of our pieces, as well as both opponent pieces
  ;;  
  ;; Using Chershire to convert json string 
  ;;   
  ;;     
  ;         

  (let [parsed (parse-string wholeJson)
        newPositionsPlayerOne (possibleMoves
                               (get-in (getFirstTokens parsed) [0 0])
                               (get-in (getFirstTokens parsed) [0 1])
                               (get parsed "spaces")
                               (get-in (get parsed "players") [0 "tokens" 1])
                               (getSecondTokens parsed))
        newPositionsPlayerTwo (possibleMoves
                               (get-in (getFirstTokens parsed) [1 0])
                               (get-in (getFirstTokens parsed) [1 1])
                               (get parsed "spaces")
                               (get-in (get parsed "players") [0 "tokens" 0])
                               (getSecondTokens parsed))
        opponentOne (possibleMoves
                     (get-in (getSecondTokens parsed) [0 0])
                     (get-in (getSecondTokens parsed) [0 1])
                     (get parsed "spaces")
                     (get-in (get parsed "players") [1 "tokens" 1])
                     (getFirstTokens parsed))
        opponentTwo (possibleMoves
                     (get-in (getSecondTokens parsed) [1 0])
                     (get-in (getSecondTokens parsed) [1 1])
                     (get parsed "spaces")
                     (get-in (get parsed "players") [1 "tokens" 0])
                     (getFirstTokens parsed))]


    (if (isGameWon (get parsed "spaces") (vec newPositionsPlayerOne) (= (getFirstCard parsed) "Pan") (getHeightAt (get parsed "spaces") (get-in (getFirstTokens parsed) [0 0]) (get-in (getFirstTokens parsed) [0 1])))
      ;; the game is won with player one, dont build and print
      (createJson 0 (isGameWon (get parsed "spaces") (vec newPositionsPlayerOne) (= (getFirstCard parsed) "Pan") (getHeightAt (get parsed "spaces") (get-in (getFirstTokens parsed) [0 0]) (get-in (getFirstTokens parsed) [0 1]))) false parsed)
      (if (isGameWon (get parsed "spaces") (vec newPositionsPlayerTwo) (= (getFirstCard parsed) "Pan") (getHeightAt (get parsed "spaces") (get-in (getFirstTokens parsed) [1 0]) (get-in (getFirstTokens parsed) [1 1])))
        ;;the game is won with player two, dont build and print
        (createJson 1 (isGameWon (get parsed "spaces") (vec newPositionsPlayerTwo) (= (getFirstCard parsed) "Pan") (getHeightAt (get parsed "spaces") (get-in (getFirstTokens parsed) [1 0]) (get-in (getFirstTokens parsed) [1 1]))) false parsed)
        (if (isGameLost (get parsed "spaces") (vec opponentOne) )
          ;;were about to lose, see if you can build there, or just lose
          (let [space (isGameLost (get parsed "spaces") (vec opponentOne) )
                buildOne (buildAtSpace (get space 0) (get space 1) (vec newPositionsPlayerOne) 1 parsed)
                buildTwo (buildAtSpace (get space 0) (get space 1) (vec newPositionsPlayerTwo) 0 parsed)]
            (if buildOne
              ;;weve saved the game! move there and build and youre done
              (createJson 0 (get buildOne 0) (get buildOne 1) parsed)
              (if buildTwo
                ;;weve saved the game! move there and build and youre done)
                (createJson 1 (get buildTwo 0) (get buildTwo 1) parsed)
                ;;weve lost! return any move who cares
                (let [moveOne (makeMove (vec newPositionsPlayerOne) 1 parsed 7)
                      moveTwo (makeMove (vec newPositionsPlayerTwo) 0 parsed 7)]

                  (if (> (get moveOne 2) (get moveTwo 2))
                    (createJson 0 (get moveOne 0) (get moveOne 1) parsed)
                    (createJson 1 (get moveTwo 0) (get moveTwo 1) parsed))))))
          (if (isGameLost (get parsed "spaces") (vec opponentTwo) )
            ;;were about to lose, see if you can build there, or just lose
            (let [space (isGameLost (get parsed "spaces") (vec opponentTwo) )
                  buildOne (buildAtSpace (get space 0) (get space 1) (vec newPositionsPlayerOne) 1 parsed)
                  buildTwo (buildAtSpace (get space 0) (get space 1) (vec newPositionsPlayerTwo) 0 parsed)]
              (if buildOne
              ;;weve saved the game! move there and build and youre done
                (createJson 0 (get buildOne 0) (get buildOne 1) parsed)
                (if buildTwo
                ;;weve saved the game! move there and build and youre done)
                  (createJson 1 (get buildTwo 0) (get buildTwo 1) parsed)
                ;;weve lost! return any move who cares
                  (let [moveOne (makeMove (vec newPositionsPlayerOne) 1 parsed 7)
                        moveTwo (makeMove (vec newPositionsPlayerTwo) 0 parsed 7)]

              ;;figure out which piece moving had the higher score, make the move and print
                    (if (> (get moveOne 2) (get moveTwo 2))
                      (createJson 0 (get moveOne 0) (get moveOne 1) parsed)
                      (createJson 1 (get moveTwo 0) (get moveTwo 1) parsed))))))
            ;;were chillin! find the best move
            (let [moveOne (makeMove (vec newPositionsPlayerOne) 1 parsed 7)
                  moveTwo (makeMove (vec newPositionsPlayerTwo) 0 parsed 7)]

              ;;figure out which piece moving had the higher score, make the move and print

              (if (> (get moveOne 2) (get moveTwo 2))
                (createJson 0 (get moveOne 0) (get moveOne 1) parsed)
                (createJson 1 (get moveTwo 0) (get moveTwo 1) parsed)))))))))

(defn -main [& args]
  (let [setupEnter (read-line)]
    (println (generate-string (setup setupEnter))))

  (while true
    (let [currMove (read-line)]
      (println (generate-string (takeTurn currMove))))))
