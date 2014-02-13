
module Trianglegame where

import Gamedata -- everything (especially failing)
import View (displayBoard, prettyShow, safeReadMoves, moveReadMessages)

import Test.HUnit (Assertion, runTestTT, assertEqual, failures, (~:))
import Data.Map.Strict (fromList, insert, size, (!))



{- Please compile or load using the -Wall flag. Thank you. -}


-- ==================================================================================

-- runs all tests in the console
rtests :: IO ()
rtests = do
        counts <- runTestTT $ "AllTests" ~: tests
        if failures counts /= 0
            then putStrLn ">> Mismatch in tests occurred!"
            else return ()
;

tests :: [Assertion]
tests = [
               assertEqual "opposingPos1" (5,3) $ opposingPos 6 4 (2,1),
               assertEqual "opposingPos2" (3,2) $ opposingPos 6 4 (0,0),
               
               assertEqual "boardSize" (6*4) $ size $ mkClearFields 6 4,
               
               assertEqual "prettyShow" prettyShowExpected $ prettyShow $ testBoard,
               
               assertEqual "neutral1" True $ neutral testBoard undefined (0,1),
               assertEqual "neutral2" False $ neutral testBoard undefined (0,0),
               assertEqual "friendly1" True $ friendly testBoard A (0,0),
               assertEqual "friendly2" False $ friendly testBoard A (0,1),
               assertEqual "friendly3" False $ friendly testBoard A (3,2),
               assertEqual "opposing1" False $ opposing testBoard A (0,0),
               assertEqual "opposing2" False $ opposing testBoard A (0,1),
               assertEqual "opposing3" True $ opposing testBoard A (3,2),
               
               {- TODO : more tests-}
               assertEqual "safeReadMoves Both1" (Right (R,R)) $ (safeReadMoves Both "R R"),
               assertEqual "safeReadMoves Both2" (failing $ moveReadMessages !! 4) $ (safeReadMoves Both "R  R")
            ]
;

testBoard :: Board
testBoard = mkBoard 6 4 "Q" "W"

prettyShowExpected :: String
prettyShowExpected = unlines [
                "",
                "    --    --    --",
                " 00 10 20 30 40 50",
                " Aa N  N  N  N  N ",
                " --    --    --   ",
                " 01 11 21 31 41 51",
                " N  N  N  N  N  N ",
                "    --    --    --",
                " 02 12 22 32 42 52",
                " N  N  N  Bb N  N ",
                " --    --    --   ",
                " 03 13 23 33 43 53",
                " N  N  N  N  N  N ",
                "    --    --    --"
            ]
;

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
        displayBoard board
        stats <- playGame board
        putStrLn "Game finished!"
        print stats
;

breakLine :: IO ()
breakLine = putStrLn "======================================"


-- keeps taking moves and shows the boards and actions until the game is over
-- it returns the results of the game when it's finished
playGame :: Board -> IO Stats
playGame = playGameWithMoves Both

playGameWithMoves :: RightsToMove -> Board -> IO Stats
playGameWithMoves playersToMove oldBoard =
            do  
                maybeAskedUnsafeMoves <- askForMoves playersToMove oldBoard
                let maybeBoard :: Failable (Maybe Stats, Board, Action, Action, RightsToMove)
                            -- its kinda hard/bad to have to change inbetween two monads in such a way....
                    maybeBoard = do 
                                    moves <- safeReadMoves playersToMove maybeAskedUnsafeMoves
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
                            putStrLn $ show (playerA newBoard) ++ " " ++ show p1Action  -- make better Action prints
                            putStrLn $ show (playerB newBoard) ++ " " ++ show p2Action
                            displayBoard newBoard
                            case maybeStats of
                                        Nothing -> playGameWithMoves nextMoveRights newBoard
                                        Just stats -> return stats
continueGame _ = error "Haskell error."
;

tryAgain :: Failable (Maybe Stats, Board, Action, Action, RightsToMove) -> Board -> IO Stats
tryAgain (Left errStr) oldBoard = do
                putStrLn errStr
                putStrLn "Try again."
                playGame oldBoard
tryAgain _ _ = error "Haskell error2."
;


askForMoves :: RightsToMove -> Board -> IO String
askForMoves playersToMove board =
            do
                putStrLn ""
                putStrLn $ "TURN " ++ show (turnCount board) ++ ": " ++ showMoveRights board playersToMove
                unsafeMoves <- getLine
                breakLine
                return unsafeMoves
;

showMoveRights :: Board -> RightsToMove -> String
showMoveRights _ Both = "Enter moves for both of you."
showMoveRights b OnlyA = "Enter the move for " ++ (pName $ playerA b)
showMoveRights b OnlyB = "Enter the move for " ++ (pName $ playerB b)
showMoveRights _ None = "Both figures are busy now. Noone may take a move. Press Enter to continue."

-- the initial Pos for Player A
initPos :: Pos
initPos = (0,0)

-- makes th board
mkBoard :: Int -> Int -> String -> String -> Board
mkBoard w h nameA nameB = let   playerAStart = initPos
                                playerBStart = opposingPos w h initPos
                          in Board {
                                        width = w,
                                        height = h,
                                        playerA = Player playerAStart nameA Nothing,
                                        playerB = Player playerBStart nameB Nothing,
                                        fields = (initialize playerAStart playerBStart $ mkClearFields w h),
                                        turnCount = 0
                                    }
;

-- makes all the neutral fields
mkClearFields :: Int -> Int -> Fields
mkClearFields w h = fromList [ ((x,y), N) | x <- [0..w-1], y <- [0..h-1] ]

-- at the beginng of the game the starting points of player A and B are already in their oppucation
initialize :: Pos -> Pos -> Fields -> Fields
initialize startA startB = insert startB B . insert startA A

-- given a starting point for a player, it calculates the most distant
-- field given the size of the board. this works because of the topology of the game board.
opposingPos :: Int -> Int -> Pos -> Pos
opposingPos w h (initX, initY)
            | odd w || odd h = error $ "invalid size! Excepted even numbers, but got " ++ show (w,h)
            | otherwise = let oppX = (initX + (w `div` 2)) `mod` w
                              oppY = (initY + (h `div` 2)) `mod` h
                          in (oppX, oppY)
;

-- given two moves, this function makes the moves and returns the actions
-- if the inputs were invalid, then a error is reported.
playMove :: Board -> (Move, Move) -> Failable (Maybe Stats, Board, Action, Action, RightsToMove)
playMove oldBoard (moveA, moveB) = do
                                    actionA <- toAction moveA A oldBoard
                                    actionB <- toAction moveB B oldBoard
                                    board' <- applyAction actionA oldBoard
                                    board'' <- applyAction actionB board'
                                    let newBoard = increaseTurnCount board''
                                        rightsToMove = rightsFromBoard newBoard
                                    return (gameStats newBoard, newBoard, actionA, actionB, rightsToMove)
;

rightsFromBoard :: Board -> RightsToMove
rightsFromBoard = undefined

toAction :: Move -> Occupation -> Board -> Failable Action
toAction mv p board = do 
                        from <- fromPos
                        let to = getAdjacentField mv from
                        (actionType, turnsToWait) <- typeAndTurns to
                        act <- mkAction actionType turnsToWait from to
                        return (act)          -- redundant return, but left staying for clarity.
            where
                fromPos :: Failable Pos
                fromPos = do 
                            player <- getPlayer p board
                            let pos = pPos player
                            return (pos)
                typeAndTurns :: Pos -> Failable (String, Int)    
                typeAndTurns to
                    | neutral board p to = return ("ConquerNeutral", 3)
                    | friendly board p to = return ("VisitFriendly", 1)
                    | opposing board p to = return ("AttackOpponent", 3)
                    | otherwise = failing "Inconsitency. Detection failure. Target field is neither friendly, opposing nor neutral."
                getPlayer :: Occupation -> Board -> Failable Player
                getPlayer A b = return (playerA b)
                getPlayer B b = return (playerB b)
                getPlayer N _ = failing "Bad argument. Neutral player does not exist."
;
mkAction :: String -> Int -> Pos -> Pos -> Failable Action
mkAction str wTurns from to
        | str == "AttackOpponent" = return $ AttackOpponent wTurns from to
        | str == "VisitFriendly"  = return $ VisitFriendly wTurns from to
        | str == "ConquerNeutral" = return $ ConquerNeutral wTurns from to
        | otherwise = failing "Bad action type name."
;

applyAction :: Action -> Board -> Failable Board
applyAction = undefined

getAdjacentField :: Move -> Pos -> Pos
getAdjacentField = undefined

posOnBoard :: Pos -> Board -> Occupation
posOnBoard pos b = fields b ! pos

neutral, friendly, opposing :: Board -> Occupation -> Pos -> Bool
neutral b _ pos = posOnBoard pos b == N
friendly b p pos = posOnBoard pos b == p
opposing b p pos = not $ friendly b p pos || neutral b p pos


finalTurns :: Int
finalTurns = 31

{-
Rules:
Each time both move to usa  a move.
A move initiates an 3-turn attack,
a 1 turn neutral takeover or a 1 turn friendly wander.

Score of both players is legth of the longest single-branches chain of
freindly fields.

-}

gameStats :: Board -> Maybe Stats
gameStats b
        | finalTurns == turnCount b = Just undefined    -- TODO: calculate winner
        | otherwise = Nothing
;

increaseTurnCount :: Board -> Board
increaseTurnCount (Board a b c d e f) = Board a b c d e (f+1)


