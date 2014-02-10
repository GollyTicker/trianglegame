import Test.HUnit hiding (test)
import Data.Map.Strict (Map, fromList, insert, size, mapWithKey, elems)

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

test :: IO ()
test = do
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
               assertEqual "opposingPos1" (5,3) $ opposingPos 6 4 (2,1),
               assertEqual "opposingPos2" (3,2) $ opposingPos 6 4 (0,0),
               assertEqual "boardSize" (6*4) $ size $ mkClearFields 6 4
            ]
;



-- =====================================================================

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
        print stats
;

playGame :: Board -> IO Stats
playGame board = do
            putStrLn $ "Turn " ++ show (turnCount board) ++ " - enter your moves:"
            unsafeMoves <- getLine
            let moves :: (Move, Move)
                moves = read unsafeMoves
                (maybeStats, newBoard, p1Action, p2Action) = playMove board moves
            putStrLn $ show (playerA board) ++ " " ++ show p1Action
            putStrLn $ show (playerB board) ++ " " ++ show p2Action
            putStrLn "Board:"
            putStrLn $ prettyShow newBoard
            case maybeStats of
                        Nothing -> playGame newBoard
                        Just stats -> return stats
;

type Pos = (Int, Int)       -- zero-based indices
type Fields = Map (Int, Int) Occupacy

data Board = Board {
                width :: Int,   -- widht and height say the number of horizontal/vertical fields
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
              } deriving (Show, Eq)
;

data Stats = Stats {
                    winner :: Player,
                    loser :: Player
                } deriving Show

data Occupacy = A | B | N deriving (Show, Eq)

data Move = L | R | V deriving (Show, Eq, Read)

data PlayerAction = Attack { stage :: Int, fromField :: Pos, toField :: Pos} |
                    MoveIntoFriendly { fromField :: Pos, toField :: Pos}     |
                    MoveIntoNeutral { stage :: Int, fromField :: Pos, toField :: Pos} deriving Show
;
-- TODO, better printing

-- the assumption is, that the x and y Positins are numbers from 0 to 9 and therefore need exactly 1 char
prettyShow :: Board -> String
prettyShow = combineShownFields . showAllFields . fieldsToInterMedfield   -- TODO

fieldsToInterMedfield :: Board -> [(Pos, Occupacy, Maybe Char)]     -- the maybe Char represents no standing player or player A or B
fieldsToInterMedfield board = addPlayers board . elems . mapWithKey initialTuple . fields $ board

initialTuple :: Pos -> Occupacy -> (Pos, Occupacy)
initialTuple p c = (p, c)

addPlayers :: Board -> [(Pos, Occupacy)] -> [(Pos, Occupacy, Maybe Char)]
addPlayers board ls = map f ls
        where
            posA = pos $ playerA $ board
            posB = pos $ playerB $ board
            f (pos, occ)
                        | posA == pos = (pos, occ, Just 'A')
                        | posB == pos = (pos, occ, Just 'B')
                        | otherwise = (pos, occ, Nothing)
;

combineShownFields = undefined

-- the first String is the first line, the second is the second
showAllFields :: [(Pos, Occupacy, Maybe Char)] -> [(Pos, String, String)]
showAllFields ls = map f ls
            where
                f (pos@(x,y), occ, maybePlayer) = (pos, show x ++ show y, show occ ++ showPlayer)
                    where
                        showPlayer = case maybePlayer of Nothing -> " "
                                                         Just a -> [a]
;

initPos :: Pos
initPos = (0,0)

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
mkClearFields w h = fromList [ ((x,y), N) | x <- [0..w-1], y <- [0..h-1] ]


initialize :: Pos -> Pos -> Fields -> Fields
initialize startA startB = insert startB B . insert startA A

opposingPos :: Int -> Int -> Pos -> Pos
opposingPos w h (initX, initY)
            | odd w || odd h = error $ "invalid size! Excepted even numbers, but got " ++ show (w,h)
            | otherwise = let oppX = (initX + (w `div` 2)) `mod` w
                              oppY = (initY + (h `div` 2)) `mod` h
                          in (oppX, oppY)

playMove :: Board -> (Move, Move) -> (Maybe Stats, Board, PlayerAction, PlayerAction)
playMove oldBoard (moveA, moveB) = (Nothing,
                                    oldBoard,
                                    MoveIntoFriendly initPos initPos,
                                    MoveIntoFriendly initPos initPos)    -- TODO
;


