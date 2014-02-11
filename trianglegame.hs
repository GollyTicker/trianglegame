
module Trianglegame where

import Gamedata -- everything
import View (prettyShow)

import Test.HUnit (Test(TestList), Assertion, runTestTT, assertEqual, failures, (~:))
import Data.Map.Strict (fromList, insert, size)



{- Please compile or load using the -Wall flag. Thank you. -}


-- ==================================================================================

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
-- TODO: sometimes one might not have a move, because one is attacking... add a Void Move?
playMove :: Board -> (Move, Move) -> Either String (Maybe Stats, Board, Action, Action)
playMove oldBoard (moveA, moveB) = do 
                                    (board', actionA) <- move moveA A oldBoard
                                    (board'', actionB) <- move moveB B board'
                                    let newBoard = increaseTurnCount board''
                                    return (gameStats newBoard, newBoard, actionA, actionB)
;

move :: Move -> Occupation -> Board -> Either String (Board, Action)
move mv p board
        | otherwise = Left "this should come now!"

finalTurns :: Int
finalTurns = 31

gameStats :: Board -> Maybe Stats
gameStats b
        | finalTurns == turnCount b = Just undefined    -- TODO: calculate winner
        | otherwise = Nothing
;

increaseTurnCount :: Board -> Board
increaseTurnCount (Board a b c d e f) = Board a b c d e (f+1)


-- ======================================================================

{-

Remainder to the usage of the either String as Error Monad.

*Trianglegame> abcd 0
Left "sorry, its too small"

*Trianglegame> abcd 3
Right 3

*Trianglegame> abcd 6
Left "its too large"

*Trianglegame> abcd 0
Left "sorry, its too small"

*Trianglegame> abcd 4
Left "I don't like four."

-}

-- abcd is a function which needs to use multiple functions that can fail.
abcd :: Integer -> Either String Integer
abcd n = do 
            x <- first n
            y <- second x
            if y == 4
                then Left "I don't like four."
                else return y
;

-- first and seocnd are functions which can fail with an error Message
first, second :: Integral a => a -> Either String a
first n
    | n <= 5 = Right n
    | otherwise = Left "its too large"
;

second n
    | n >= 1 = Right n
    | otherwise = Left "sorry, its too small"
;



