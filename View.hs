
module View (displayBoard, prettyShow, safeReadMoves, moveReadMessages, displayPaths) where

import Types -- everything (especially failing)

import Data.Map.Strict (elems, mapWithKey)
import Data.List (sortBy, groupBy, intercalate, mapAccumL)
import Data.Ord (comparing)


safeReadMoves :: RightsToMove -> String -> Failable (Move, Move)
safeReadMoves rights maybeAskedUnsafeMoves = safeRead rights $ prepareInput maybeAskedUnsafeMoves


-- removes leading and tailing whitespace
prepareInput :: String -> String
prepareInput =  reverse
                . chomp -- remove tailing whitespace
                . reverse
                . chomp -- remove leading whitespace
; -- remember, the execution order is bottom to top


chomp :: String -> String
chomp = dropWhile (==' ')

safeRead :: RightsToMove -> String -> Failable (Move, Move)
safeRead Both [p1, ' ', p2] = do 
                                move1 <- fromChar p1
                                move2 <- fromChar p2
                                return (move1, move2)
;
safeRead OnlyA [p1] = do 
                        mv1 <- fromChar p1
                        return (mv1, Nil)
;
safeRead OnlyB [p2] = do 
                        mv2 <- fromChar p2
                        return (Nil, mv2)
;
safeRead None [] = return (Nil, Nil)


safeRead _ ('\"':_) = failing $ moveReadMessages !! 0
safeRead None _ = failing $ moveReadMessages !! 1
safeRead OnlyB _ = failing $ moveReadMessages !! 2
safeRead OnlyA _ = failing $ moveReadMessages !! 3
safeRead Both _ = failing $ moveReadMessages !! 4

moveReadMessages :: [String]
moveReadMessages = [
                    {-0-} "I cannnot understand you. Please ommit the quotes.",
                    {-1-} "Noone must make a move.",
                    {-2-} "I cannnot understand you. Please write only R, L or V.",
                    {-3-} "I cannnot understand you. Please write only R, L or V.",
                    {-4-} "I cannnot understand you. Please write something like R L, L R, V R, etc...",
                    {-5-} "Invalid letters. Please use R, L or V."
                   ]
;


fromChar :: Char -> Failable Move
fromChar c = case reads [c] of
                    [(a,_)] -> return a
                    _ -> failing $ moveReadMessages !! 5
;

-- =======================================================================================


displayBoard :: Board -> IO ()
displayBoard b = do
                putStrLn "Board:"
                putStrLn $ prettyShow b
;


displayPaths :: ((String, Path), (String, Path)) -> IO ()
displayPaths ((p1Name, path1),(p2Name, path2)) = undefined


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

-- helper function to get the specific coordinate in that tuple
yCoord :: (Pos, b, c) -> Int
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
combineAllRows w xs = (newLine ++) .  indentation . intercalate newLine .  repeatFirstLine . map (addBars w) $ xs
        where
            indentation :: String -> String
            indentation = unlines . map (' ' :) . lines
;

repeatFirstLine :: [String] -> [String]
repeatFirstLine xs = xs ++ [takeWhile (/='\n') (head xs)]

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
;