
module Trianglegame where

import Gamedata
import View (prettyShow)

import Test.HUnit (Test(TestList), Assertion, runTestTT, assertEqual, failures, (~:))
import Data.Map.Strict (fromList, insert, size)

{- Please compile or load using the -Wall flag. Thank you. -}



-- runs all tests in the console
runTests :: IO ()
runTests = do
        counts <- runTestTT $ TestList unitTests
        if failures counts /= 0
            then putStrLn ">> Mismatch in tests occurred!"
            else return ()
;

unitTests :: [Test]
unitTests = [
                       "testBoard" ~:  testBoard
                    ]
;

testBoard :: [Assertion]
testBoard = [
               assertEqual "opposingPos1" (5,3) $ opposingPos 6 4 (2,1),
               assertEqual "opposingPos2" (3,2) $ opposingPos 6 4 (0,0),
               assertEqual "boardSize" (6*4) $ size $ mkClearFields 6 4
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
        putStrLn ">> Enter moves as (Player1Move, Player2Move)"
        putStrLn ">> e.g. (R, V)"
        putStrLn ">> Moves can be to the left (L), right(R) or vertically (V)."
        putStrLn "Game starts now!"
        let board = mkBoard 6 4 n1 n2
        stats <- playGame board
        putStrLn "Game finished!"
        print stats
;


-- keeps taking moves and shows the boards and actions until the game is over
-- it returns the results of the game when it's finished
playGame :: Board -> IO Stats
playGame board = do
            putStrLn ""
            putStrLn $ "Turn " ++ show (turnCount board) ++ " - enter your moves:"
            unsafeMoves <- getLine
            let moves :: (Move, Move)
                moves = read unsafeMoves
                maybeBoard = playMove board moves
                Left errStr = maybeBoard
                Right (maybeStats, newBoard, p1Action, p2Action) = maybeBoard
            if isError maybeBoard
                then do putStrLn errStr
                        putStrLn "Try again."
                        playGame board
                else do 
                        putStrLn $ show (playerA board) ++ " " ++ show p1Action  -- make better Action prints
                        putStrLn $ show (playerB board) ++ " " ++ show p2Action
                        putStrLn "Board:"
                        putStrLn $ prettyShow newBoard
                        case maybeStats of
                                    Nothing -> playGame newBoard
                                    Just stats -> return stats
;

isError :: Either String b -> Bool
isError (Left _) = True
isError (Right _) = False


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
                                        playerA = Player playerAStart nameA,
                                        playerB = Player playerBStart nameB,
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

-- given an starting point for a player, it calculates the most distant
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
-- TODO: sometimes one might not have a move, because one is attacking... add a Void Move.
playMove :: Board -> (Move, Move) -> Either String (Maybe Stats, Board, PlayerAction, PlayerAction)
playMove oldBoard (moveA, moveB) = Right (Nothing,
                                    oldBoard,
                                    MoveIntoFriendly initPos initPos,
                                    MoveIntoFriendly initPos initPos)    -- TODO
;


