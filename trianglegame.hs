import Test.HUnit
import Data.Map.Strict (Map, fromList, insert)

-- (<<) is a infix short of map -- should have a low precedence
infixr 5 <<
(<<) :: (a -> b) -> [a] -> [b]
f << ls = map f ls
-- Usage: 
-- *main> (*21) << [-2..6]
-- [-42,-21,0,21,42,63,84,105,126]

infix 1 <<<
infix 1 >>>
(<<<), printLs :: Show b => (a -> b) -> [] a -> IO ()
(>>>) :: Show b => [] a -> (a -> b) -> IO ()
printLs f = mapM_ (print . f)
f <<< ls = printLs f ls
ls >>> f = printLs f ls
-- Sample usage:
-- *main> (\n -> combinations n "acbd") <<< [0..4]
-- [""]
-- ["a","c","b","d"]
-- ["ac","ab","ad","cb","cd","bd"]
-- ["acb","acd","abd","cbd"]
-- ["acbd"]
-- Or: *main> [0..4] >>> \n -> combinations n "acbd"

-- Helpful for drawing on console
draw :: Integral a => a -> String
draw n = replicate (fromIntegral n) '*'

dot,(.:) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
dot = (.) (.) (.)   -- http://www.haskell.org/haskellwiki/Pointfree#Dot
(.:) = dot
infixr 9 `dot`
infixr 9 .:

-- ================================================================

tests :: IO ()
tests = do
        putStrLn ">> HUnit tests..."
        counts <- runTestTT $ TestList unitsProblemTests
        if failures counts /= 0
            then putStrLn ">> Tests failed."
            else return ()
;

unitsProblemTests = [
                       "testBoard" ~:  testBoard
                    ]
;

testBoard = [
               assertEqual "" 0 0
            ]
;



-- =====================================================================

main = do
        putStrLn "First Player Name:"
        n1 <- getLine
        putStrLn "Second Player Name:"
        n2 <- getLine
        putStrLn "How to enter moves:"
        putStrLn "Enter Moves as (Player1Move, Player2Move)"
        putStrLn "e.g. (R, V)"
        putStrLn "Moves can be to the left (L), right(R) or vertically (V)."
        putStrLn "Game starts now!"
        let board = mkBoard 6 4 n1 n2
        stats <- playGame board
        print stats
;

playGame :: Board -> IO Stats
playGame board = do
            putStrLn $ "Turn " ++ show (turnCount board) ++ " - enter your moves."
            unsafeMoves <- getLine
            let moves :: (Move, Move)
                moves = read unsafeMoves
                (maybeStats, newBoard, p1Action, p2Action) = playMove board moves
            putStrLn $ show (playerA board) ++ ":"
            print p1Action
            putStrLn $ show (playerB board) ++ ":"
            print p2Action
            putStrLn "Board:"
            putStrLn $ prettyShow newBoard
            case maybeStats of
                        Nothing -> playGame newBoard
                        Just stats -> return stats
;

type Pos = (Int, Int)
type Fields = Map (Int, Int) Occupacy

data Board = Board {
                width :: Int,
                height :: Int,
                playerA :: Player,
                playerB :: Player,
                fields :: Map Pos Occupacy,
                turnCount :: Int
            } deriving Show
;

data Player = Player {
                    pos :: Pos,
                    name :: String
                    -- more ....
              } deriving (Show, Eq)
;

data Stats = Stats {
                    winner :: Player,
                    loser :: Player
                } deriving Show

data Occupacy = A | B | Neutral deriving (Show, Eq)

data Move = L | R | V deriving (Show, Eq, Read)

data PlayerAction = Attack { stage :: Int, from :: Player, fromField :: Pos, toField :: Pos} |
                    MoveIntoFreindly { from :: Player, fromField :: Pos, toField :: Pos}     |
                    MoveIntoNeutral { stage :: Int, from :: Player, fromField :: Pos, toField :: Pos} deriving Show
;


prettyShow :: Board -> String
prettyShow = show   -- TODO

initPos :: Pos
initPos = (1,1)

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

mkClearFields :: Int -> Int -> Fields
mkClearFields w h = fromList [ ((x,y), Neutral) | x <- [1..w], y <- [1..h] ]


initialize :: Pos -> Pos -> Fields -> Fields
initialize startA startB = insert startB B . insert startA A

opposingPos :: Int -> Int -> Pos -> Pos
opposingPos w h (initX, initY)
            | odd w || odd h = error "invalid size! Excepted even numbers, but got " ++ show (w,h)
            | otherwise = undefined

playMove :: Board -> (Move, Move) -> (Maybe Stats, Board, PlayerAction, PlayerAction)
playMove = undefined


