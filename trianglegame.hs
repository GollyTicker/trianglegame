
module Trianglegame where

import Gamedata -- everything
import View (prettyShow)

import Test.HUnit (Test(TestList), Assertion, runTestTT, assertEqual, failures, (~:))
import Data.Map.Strict (fromList, insert, size, (!))



{- Please compile or load using the -Wall flag. Thank you. -}


-- ==================================================================================

-- runs all tests in the console
rtests :: IO ()
rtests = do
        counts <- runTestTT $ TestList unitTests
        if failures counts /= 0
            then putStrLn ">> Mismatch in tests occurred!"
            else return ()
;

unitTests :: [Test]
unitTests = [
               "tests" ~:  tests
            ]
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
               assertEqual "opposing3" True $ opposing testBoard A (3,2)
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
playGame :: Board -> IO Stats   -- TODO: use Faiable here instead of manual error detection
playGame board =
            do
                putStrLn ""
                putStrLn $ "Turn " ++ show (turnCount board) ++ " - enter your moves:"
                unsafeMoves <- getLine
                let moves :: (Move, Move)
                    moves = read unsafeMoves    -- failable
                    maybeBoard = playMove board moves   -- failable
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

isError :: Failable b -> Bool
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
playMove :: Board -> (Move, Move) -> Failable (Maybe Stats, Board, Action, Action)
playMove oldBoard (moveA, moveB) = do
                                    actionA <- mkAction moveA A oldBoard
                                    actionB <- mkAction moveB B oldBoard
                                    board' <- applyAction actionA oldBoard
                                    board'' <- applyAction actionB board'
                                    let newBoard = increaseTurnCount board''
                                    return (gameStats newBoard, newBoard, actionA, actionB)
;

mkAction :: Move -> Occupation -> Board -> Failable Action
mkAction mv p board = do 
                        from <- fromPos
                        let to = getAdjacentField mv from
                        (actionType, turnsToWait) <- typeAndTurns to
                        act <- mkAction' actionType turnsToWait from to
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
                    | otherwise = fail "Inconsitency. Detection failure. Target field is neither friendly, opposing nor neutral."
                getPlayer :: Occupation -> Board -> Failable Player
                getPlayer A b = return (playerA b)
                getPlayer B b = return (playerB b)
                getPlayer N _ = fail "Bad argument. Neutral player does not exist."
;
mkAction' :: String -> Int -> Pos -> Pos -> Failable Action
mkAction' = undefined

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


-- ======================================================================

{-

Remainder to the usage of Failable as Error Monad. (as Either String)

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
abcd :: Integer -> Failable Integer
abcd n = do 
            x <- first n
            y <- second x
            if y == 4
                then fail "I don't like four."
                else return y
;

-- first and second are functions which can fail with an error Message
first, second :: Integral a => a -> Failable a
first n
    | n <= 5 = return n
    | otherwise = fail "its too large"
;

second n
    | n >= 1 = return n
    | otherwise = fail "sorry, its too small"
;



