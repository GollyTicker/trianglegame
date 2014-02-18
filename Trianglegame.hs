
module Trianglegame (
                      main
                    , opposingPos
                    , mkClearFields
                    , friendly
                    , neutral
                    , opposing
                    , initPos
                    , paths
                    , updateField
                    , adjacent
                    , longestPath
                    , mkBoard
                    , playMove
                    , module Types
                    , module View
                    ) where -- export everything for tests

import Types -- everything , especially failing :: String -> Failable a
import View (displayBoard, prettyShow, safeReadMoves, moveReadMessages, displayPaths, prettyPrintStats, displayPlayerMakingAction)
import Logic (
                mkBoard
              , longestPath
              , playMove
              , friendly
              , opposing
              , neutral
              , initPos
              , paths
              , updateField
              , adjacent
              , mkClearFields
              , opposingPos
            )


{- Please compile or load using the -Wall flag. -}

-- =====================================================================

-- start a game
main :: IO ()
main = do
        putStrLn "First Player Name:"
        n1 <- getLine
        putStrLn "Second Player Name:"
        n2 <- getLine
        breakLine
        putStrLn $ ">> Enter moves in form of \"" ++ n1 ++ "sMove " ++ n2 ++ "sMove\""
        putStrLn ">> Moves can be to the left (L), right(R) or vertically (V)."
        putStrLn ">> e.g. \"R V\""
        putStrLn ">> or \"R\" if only one player may move."
        breakLine
        putStrLn "Game starts now!"
        let board = mkBoard 6 4 n1 n2
        displayGame board
        stats <- playGame board
        putStrLn "Game finished!"
        breakLine
        prettyPrintStats stats
;

breakLine :: IO ()
breakLine = putStrLn "======================================"

displayGameWithActions :: (Board, Action, Action) -> IO ()
displayGameWithActions (b, act1, act2) =
                do 
                    displayBoard b
                    displayPlayerMakingAction (playerA b) act1
                    displayPlayerMakingAction (playerB b) act2
                    putStrLn ""
                    displayPaths $ getPathsAndPlayerNames $ b
;

displayGame :: Board -> IO ()
displayGame b =
                do 
                    displayBoard b
                    displayPaths $ getPathsAndPlayerNames $ b
;

getPathsAndPlayerNames :: Board -> ((String, Path),(String, Path))
getPathsAndPlayerNames b = let first  = (pName $ playerA b, longestPath A b)
                               second = (pName $ playerB b, longestPath B b)
                           in (first, second)
;

-- keeps taking moves and shows the boards and actions until the game is over
-- it returns the results of the game when it's finished
playGame :: Board -> IO Stats
playGame = playGameWithMoves Both

playGameWithMoves :: RightsToMove -> Board -> IO Stats
playGameWithMoves playersToMove oldBoard =
            do  
                moves <- askForMoves playersToMove oldBoard
                let maybeBoard :: Failable (Maybe Stats, Board, Action, Action, RightsToMove)
                            -- its kinda hard/bad to have to change inbetween two monads in such a way....
                    maybeBoard = do 
                                    afterMove <- playMove oldBoard moves
                                    return (afterMove)      -- (redundant return)
                if failed maybeBoard    -- at this point, all the errors are finally catched and are handled.
                    then tryAgain maybeBoard oldBoard
                    else continueGame maybeBoard
;

failed :: Failable b -> Bool
failed (Left _) = True
failed (Right _) = False

continueGame :: Failable (Maybe Stats, Board, Action, Action, RightsToMove) -> IO Stats
continueGame (Right (maybeStats, newBoard, p1Action, p2Action, nextMoveRights)) = 
                        do 
                            displayGameWithActions (newBoard, p1Action, p2Action)
                            breakLine
                            case maybeStats of
                                        Nothing -> playGameWithMoves nextMoveRights newBoard    -- here is the recursive call.
                                        Just stats -> return stats
continueGame _ = error "Haskell impossible case 1"
;

tryAgain :: Failable (Maybe Stats, Board, Action, Action, RightsToMove) -> Board -> IO Stats
tryAgain (Left errStr) oldBoard = do
                putStrLn errStr
                putStrLn "Try again."
                playGame oldBoard
tryAgain _ _ = error "Haskell impossible case 2"
;


askForMoves :: RightsToMove -> Board -> IO (Move,Move)
askForMoves playersToMove board =
            do
                putStrLn ""
                putStrLn $ "TURN " ++ show (turnCount board) ++ ": " ++ showMoveRights board playersToMove
                unsafeMoves <- getLine
                let promptedMoves = safeReadMoves playersToMove unsafeMoves
                    retryFailed errMsg = do 
                                            putStrLn errMsg
                                            moves <- askForMoves playersToMove board
                                            return moves
                    acceptSucceded moves = do
                                        breakLine
                                        return moves
                moves <- either retryFailed acceptSucceded promptedMoves
                return (moves)
;

showMoveRights :: Board -> RightsToMove -> String
showMoveRights _ Both = "Enter moves for both of you."
showMoveRights b OnlyA = "Enter the move for " ++ (pName $ playerA b)
showMoveRights b OnlyB = "Enter the move for " ++ (pName $ playerB b)
showMoveRights _ None = "Both figures are busy now. Noone may take a move. Press Enter to continue."


