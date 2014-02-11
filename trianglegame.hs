import Test.HUnit (Test(TestList), Assertion, runTestTT, assertEqual, failures, (~:))
import Data.Map.Strict (Map, fromList, insert, size, mapWithKey, elems)
import Data.List (sortBy, groupBy, intercalate, mapAccumL)
import Data.Ord (comparing)



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
                        putStrLn $ show (playerA board) ++ " " ++ show p1Action
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

type Pos = (Int, Int)       -- zero-based indices

-- short-cut for the Map
type Fields = Map Pos Occupation

data Board = Board {
                width :: Int,   -- widht and height say the number of horizontal/vertical fields
                height :: Int,
                playerA :: Player,
                playerB :: Player,
                fields :: Fields, -- each field is either neutral or As field or Bs field.
                turnCount :: Int    -- counts the turns already moved on the board
            } deriving Show
;

-- each field is either neutral or As field or Bs field.
data Occupation = A | B | N deriving (Show, Eq)

-- currently a Player only has a Position and a name
data Player = Player {
                    pPos :: Pos,
                    pName :: String
              } deriving (Show, Eq)
;

-- currently only holds the winning Player
data Stats = Stats {
                    winner :: Player
                } deriving Show
;

-- Seen from the quadratic representation, a move goes either to the left or to teh right or vertically.
-- whether it goes to the top or bottom is dependent form the actual field.
data Move = L | R | V deriving (Show, Eq, Read)

data PlayerAction = Attack { stage :: Int, fromField :: Pos, toField :: Pos} |
                    MoveIntoFriendly { fromField :: Pos, toField :: Pos}     |
                    MoveIntoNeutral { stage :: Int, fromField :: Pos, toField :: Pos} deriving Show
;

-- quadratic representation of the baords
-- an assumption is, that the x and y Positions are numbers from 0 to 9 and therefore need exactly 1 char
prettyShow :: Board -> String
prettyShow board = combineShownFields (width board) . showAllFields . fieldsToIntermedfield $ board


-- field field is getting mapped to a Tuple containting the information to be shown
-- it first creates a list of [(Pos, Occupation)] and then adds the Players into them
fieldsToIntermedfield :: Board -> [(Pos, Occupation, Maybe Char)]     -- the maybe Char represents no standing player or player A or B
fieldsToIntermedfield board = addPlayers board . elems . mapWithKey (\pos occ -> (pos, occ)) . fields $ board

-- adds the players at their respective Tuples
addPlayers :: Board -> [(Pos, Occupation)] -> [(Pos, Occupation, Maybe Char)]
addPlayers board ls = map f ls
        where
            posA = pPos $ playerA $ board
            posB = pPos $ playerB $ board
            f (pos, occ)
                    | posA == pos = (pos, occ, Just 'a') -- TODO use the first differing character of the given names here
                    | posB == pos = (pos, occ, Just 'b') --      use the first differing character of the given names here
                    | otherwise = (pos, occ, Nothing)
;


-- creates intermediate Strings for the latter join
-- the first String is the first line, the second String is the second line
showAllFields :: [(Pos, Occupation, Maybe Char)] -> [(Pos, String, String)]
showAllFields ls = map f ls
            where
                f (pos@(x,y), occ, maybePlayer) = (pos, show x ++ show y, show occ ++ showPlayer)
                    where
                        showPlayer = case maybePlayer of Nothing -> " "
                                                         Just a -> [a]
;

newLine :: String
newLine = "\n"

-- fst for Tuple3
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- helper function ot get the specific coordinate in that tuple
xCoord, yCoord :: (Pos, b, c) -> Int
xCoord = fst . fst3
yCoord = snd . fst3

-- combines all the intermediate Strings at the desired positions
combineShownFields :: Int -> [(Pos, String, String)] -> String
combineShownFields w xs = combineAllRows w . formRows . sortBy (comparing yCoord) . sortBy (comparing fst3) $ xs


formRows :: [(Pos, String, String)] -> [(Int, String)]
formRows xs = map f . groupBy (\a b -> yCoord a == yCoord b) $ xs   -- group by equal y coord
            where
                f :: [(Pos, String, String)] -> (Int, String)
                f ls = let (_, fstLines, sndLines) = unzip3 ls
                       in (yCoord $ head ls, intercalate " " fstLines ++ newLine ++ intercalate " " sndLines)
;

-- an assumption is, that the triangle for the field (0,0) points downwards. that means that (0,0) and (0,h-1) are directly connected
combineAllRows :: Int -> [(Int, String)] -> String
combineAllRows w xs = intercalate newLine . map (addBars w) $ xs


-- adds horizontal bars (minuses) inbetween an upper and lower field if they're not directly connected
-- it adds the bars above the current Position and uses the facts taht each field is two chars wide and
-- that inbetween two fields a single blank is inserted
addBars :: Int -> (Int, String) -> String
addBars w (y, str)                                -- (length $ takeWhile (/='\n') str) should be equal to (width*3 - 1)
            | even y    = combine . snd . mapAccumL step tokens        $ [0..(w*3 - 1) - 1]
            | otherwise = combine . snd . mapAccumL step (tail tokens) $ [0..(w*3 - 1) - 1]
        where
            step :: [Char] -> Int -> ([Char], Char)
            step tks idx
                        | idx `mod` 3 == 0 = (tks, head tks)   -- first character of a field
                        | idx `mod` 3 == 1 = (tks, head tks)   -- second character of a field
                        | idx `mod` 3 == 2 = (tail tks, ' ')   -- these are the empty spaces inbetween coloumns of fields
                        | otherwise = error "duh."
            combine :: String -> String
            combine = (++ newLine ++ str)
            tokens = ' ' : '-' : tokens

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


