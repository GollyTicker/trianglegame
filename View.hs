
module View (prettyShow) where

import Gamedata

import Data.Map.Strict (elems, mapWithKey)
import Data.List (sortBy, groupBy, intercalate, mapAccumL)
import Data.Ord (comparing)


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