
module Trianglegame where

import Types -- everything , especially failing :: String -> Failable a
import View (displayBoard, prettyShow, safeReadMoves, moveReadMessages, displayPaths, prettyPrintStats)

import Test.HUnit (Assertion, runTestTT, assertEqual, failures, (~:))
import Data.Map.Strict as Map (fromList, toList, filterWithKey, insert, size, (!))
import qualified Data.Map.Strict as Map (filter)
import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.Set as Set (Set, fromList, toList, insert, delete, union, intersection, (\\))
import Control.Applicative ((<$>), (<*>))

import Debug.Trace (trace)



{- Please compile or load using the -Wall flag. -}


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
               assertEqual "opposingPos1" (5,3) $ opposingPos 6 4 (2,1)
              ,assertEqual "opposingPos2" (3,2) $ opposingPos 6 4 (0,0)
               
              ,assertEqual "boardSize" (6*4) $ size $ mkClearFields 6 4
               
              ,assertEqual "prettyShow" prettyShowExpected $ prettyShow $ initialBoard
               
              ,assertEqual "neutral1" True $ neutral initialBoard undefined (0,1)
              ,assertEqual "neutral2" False $ neutral initialBoard undefined (0,0)
              ,assertEqual "friendly1" True $ friendly initialBoard A (0,0)
              ,assertEqual "friendly2" False $ friendly initialBoard A (0,1)
              ,assertEqual "friendly3" False $ friendly initialBoard A (3,2)
              ,assertEqual "opposing1" False $ opposing initialBoard A (0,0)
              ,assertEqual "opposing2" False $ opposing initialBoard A (0,1)
              ,assertEqual "opposing3" True $ opposing initialBoard A (3,2)
               
               {- TODO : more tests-}
              ,assertEqual "safeReadMoves Both1" (Right (R,R)) $ (safeReadMoves Both "R R")
              ,assertEqual "safeReadMoves Both2" (failing $ moveReadMessages !! 4) $ (safeReadMoves Both "R  R")
               
               
               -- path tests
              ,assertEqual "paths at beginning" (Set.fromList [[initPos]]) $ paths A initialBoard
              ,assertEqual "paths at beginning" (Set.fromList [[opposingPos 6 4 initPos]]) $ paths B initialBoard
              ,assertEqual "simple Paths"
                                        (Set.fromList [[(0,0), (1,0), (2,0)], [(1,2), (2,2), (3,2)]])
                                        $ paths A
                                        $ insertFields [(0,0), (1,0), (2,0), (1,2), (2,2), (3,2)]
              ,assertEqual "simple path was crossed by opponent"
                                        (Set.fromList [[(0,0), (1,0), (2,0)], [(1,2)], [(3,2)]])
                                        $ paths A
                                        $ updateField (2,2) B
                                        $ insertFields [(0,0), (1,0), (2,0), (1,2), (2,2), (3,2)]
              ,assertEqual "a cycle's fields are only counted once 1"
                                        (Set.fromList [[(0,0), (1,0), (2,0), (3,0), (4,0), (5,0)]])
                                        $ paths A $ insertFields [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0)] {- fill the first row with A -}
              ,assertEqual "a cycle's fields are only counted once 2"
                                        (Set.fromList [[(0,0),(0,3),(1,3),(2,3),(2,0),(1,0)]])
                                        $ paths A
                                        $ insertFields [(0,0), (1,0), (2,0), (2,3), (1,3), (0,3)] {- make a small cycle -}
              ,assertEqual "multiple branched paths should degenerate into simple paths"
                                        (Set.fromList [[(1,0),(2,0)],[(5,0)],[(0,3)]])
                                        $ paths A $ insertFields [(0,0),(1,0),(5,0),(0,3),(2,0)]
              ,assertEqual "adjacent a b <-> adjacent b a" True
                        $ all (\(a, b) -> if (b `elem` adjacent a initialBoard) then (b `elem` adjacent a initialBoard) else True)
                        $ (\ps -> (,) <$> ps <*> ps)
                        $ [(x,y)|x <- [0..6-1], y <- [0..4-1]]
            ]
;


symmDiff :: Ord a => Set a -> Set a -> Set a
symmDiff a b = (a `union` b) \\ (a `intersection` b)

initialBoard :: Board
initialBoard = mkBoard 6 4 "Q" "W"

-- USEFUL FOR TESTS!
progressedGame :: Failable Board
progressedGame = return initialBoard
                    >>= playMove' (R,R)
                    >>= playMove' (R,R)
                    >>= playMove' (R,R)
                    -- more...
                    >>= playMove' (R,R)
;

insertFields :: [Pos] -> Board
insertFields xs = foldl (\b pos -> updateField pos A b) initialBoard xs

-- a simpler version for making moves for testing purposes.
playMove' :: (Move, Move) -> Board -> Failable Board
playMove' moves b = playMove b moves >>= (\(_, newBoard, _, _, _) -> return newBoard)

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
        displayGame board
        stats <- playGame board
        putStrLn "Game finished!"
        breakLine
        prettyPrintStats stats
;

breakLine :: IO ()
breakLine = putStrLn "======================================"

displayGame :: Board -> IO ()
displayGame b = do 
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
                            displayGame newBoard
                            breakLine
                            case maybeStats of
                                        Nothing -> playGameWithMoves nextMoveRights newBoard
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


-- ==========================================================================================

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
                                        playerA = Player playerAStart nameA Nothing A,
                                        playerB = Player playerBStart nameB Nothing B,
                                        fields = (initialize playerAStart playerBStart $ mkClearFields w h),
                                        turnCount = 0
                                    }
;

-- checks that:
{-
The field both players are standing on are indeed theirs.
Thats the players cannot move into the same
field simultanously. (Thats means, that after all the currently runnings acitons are over,
that both players are an odd number of moves away from each other)
Also, the Set of Paths is never empty!

These invariants should be always true.
They are not mistakes by the player, but bugs in the logic.

Failure Messegaes with "Inconsistency" should NOT blubble up to the player!
There of the same type and should never arise!
-}
checkInvariants :: Board -> Failable Board
checkInvariants = undefined

-- returns all the paths for a specific player(given as Occupation)
-- first, a new Board is made, where all the fields
-- with three neighbors are made neutral.
-- then it starts. this destroyes paths with multiple branches.
-- Also, this has the effect, that all fields now have
-- atmost two adjacent freindly neighbors.
-- it starts with all singleton fields and
-- repeatedly merges connected paths
-- once all there are no more paths to merge,
-- the remaining set is returned
paths :: Occupation -> Board -> Set Path
paths player b = {-trace ("filteredOpposing:" ++ show filteredOpposing)-} finalPaths
        where
            deletedMultipleBranches :: Fields
            deletedMultipleBranches = Map.filterWithKey threeNeighbors $ fields b
            threeNeighbors :: Pos -> Occupation -> Bool
            threeNeighbors pos occ = 3 /= (length $ filter (\x -> occupiedBy x b == occ) $ adjacent pos b)
            filteredOpposing :: Fields
            filteredOpposing = Map.filter (==player)
                                $ deletedMultipleBranches
            singletonPaths :: Set Path
            singletonPaths = Set.fromList $ map (return . fst) $ Map.toList $ filteredOpposing
            finalPaths = mergeWhileConnected singletonPaths b
;

mergeWhileConnected :: Set Path -> Board -> Set Path
mergeWhileConnected ps b = case maybeConnectedPaths ps b of
                                Just (newPaths) -> mergeWhileConnected newPaths b
                                Nothing -> ps
;

maybeConnectedPaths :: Set Path -> Board -> Maybe (Set Path)
maybeConnectedPaths ps b = do 
                                (firstPath, secondPath) <- connectable ps b
                                return $ connect (firstPath, secondPath) ps
;

adjacent :: Pos -> Board -> [Pos]
adjacent pos b = [ getAdjacentField b mv pos | mv <- [L,R,V]]

connectable :: Set Path -> Board -> Maybe (Path, Path)
connectable psSet board = maybeConnection
        where
            ps :: [Path]
            ps = Set.toList $ psSet
            pairs :: [(Path, Path)] -- excluding the identical path 
            pairs = filter (\(a,b) -> a /= b) $ (,) <$> ps <*> ps
            connectables :: [(Path, Path)]
            connectables = filter (\(a,b) -> a `connectsTo` b) pairs
            connectsTo :: Path -> Path -> Bool
            fstPath `connectsTo` sndPath = (last fstPath) `elem` adjacent (head sndPath) board
            maybeConnection :: Maybe (Path, Path)
            maybeConnection
                    | null connectables = Nothing
                    | otherwise = return $ head connectables
;

connect :: (Path, Path) -> Set Path -> Set Path
connect (p1, p2) ps =   Set.insert (p1 ++ p2)
                        . Set.delete p1
                        . Set.delete p2
                        $ ps
;

longestPath :: Occupation -> Board -> Path
longestPath p b = maximumBy (comparing length) $ Set.toList $ paths p b

-- makes all the neutral fields
mkClearFields :: Int -> Int -> Fields
mkClearFields w h = Map.fromList [ ((x,y), N) | x <- [0..w-1], y <- [0..h-1] ]

-- at the beginng of the game the starting points of player A and B are already in their oppucation
initialize :: Pos -> Pos -> Fields -> Fields
initialize startA startB = Map.insert startB B . Map.insert startA A

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
                                    board1 <- applyAction oldBoard actionA
                                    board2 <- applyAction board1 actionB
                                    let board3 = increaseTurnCount board2
                                        newBoard = decreaseActionTurns board3
                                        rightsToMove = rightsFromBoard newBoard
                                    safeNewBoard <- checkInvariants newBoard
                                    return (isFinished safeNewBoard, safeNewBoard, actionA, actionB, rightsToMove)
;

rightsFromBoard :: Board -> RightsToMove
rightsFromBoard b = case (actionOf A b, actionOf B b) of
                            (Just _, Just _) -> None
                            (Just _, Nothing) -> OnlyB
                            (Nothing, Just _) -> OnlyA
                            (Nothing, Nothing) -> Both
;

-- TODO test
toAction :: Move -> Occupation -> Board -> Failable Action
toAction mv p board
            | actionActive && nilMove = return action
            | not actionActive && not nilMove = do 
                                                    act <- mkAction actionType turnsToWait from to
                                                    return (act)          -- redundant return, but left staying for clarity.
            | actionActive && not nilMove = failing "Game Inconsistency. Action active, but got a move command."
            | not actionActive && nilMove = failing "Game Inconsistency. Action inactive, but got a nilMove."
            | otherwise = error "Haskell impossible case 5"
            where
                from :: Pos
                from = pPos $ player
                to :: Pos
                to = getAdjacentField board mv from
                (actionType, turnsToWait)
                    | neutral board p to = ("ConquerNeutral", 3)
                    | friendly board p to = 
                                    if ((opponentOf p) `attacking` to) board
                                        then ("DefendField", 1)
                                        else ("VisitFriendly", 1)
                    | opposing board p to = ("AttackOpponent", 3)
                    | otherwise = error "Haskell impossible case 3"
                player = playerByOcc p board
                nilMove = (mv == Nil)
                mAction = continuedAction player
                actionActive = not $ mAction == Nothing
                Just action = mAction
;

attacking :: Occupation -> Pos -> Board -> Bool
attacking p pos b = case actionOf p b of  Just (AttackOpponent _ _ target) -> target == pos
                                          _ -> False
;

playerByOcc :: Occupation -> Board -> Player
playerByOcc A b = playerA b
playerByOcc B b = playerB b
playerByOcc _ _ = error "bendnfmgi"

actionOf :: Occupation -> Board -> Maybe Action
actionOf p b = continuedAction $ playerByOcc p b

opponentOf :: Occupation -> Occupation
opponentOf A = B
opponentOf B = A
opponentOf N = error "asdasdasd"

mkAction :: String -> Int -> Pos -> Pos -> Failable Action
mkAction str wTurns from to
        | str == "AttackOpponent" = return $ AttackOpponent wTurns from to
        | str == "VisitFriendly"  = return $ VisitFriendly wTurns from to
        | str == "ConquerNeutral" = return $ ConquerNeutral wTurns from to
        | str == "DefendField" = return $ DefendField from to
        | otherwise = error "Haskell impossible case 4"
;

-- decrease the counter for the turns.
decreaseActionTurns :: Board -> Board
decreaseActionTurns = updatePlayerActions (>>=f)
        where
            f :: Action -> Maybe Action
            f act
                | waitTurns act == 0 = Nothing
                | otherwise = return act
;

-- given an aciton. It's being aplied,
-- if all its turns to go are over.
applyAction :: Board -> Action -> Failable Board
{-          -- this maaay get used lateron. but for now, its still Failable.
applyAction b act | invalidAction = failing "TODO"
            where
                invalidAction = undefined
-}
applyAction b (AttackOpponent n source target)
                        | n == 0 = let player = (occupiedBy source b)
                                   in return $ (player `invades` target) b
                        | otherwise = return b
applyAction b (VisitFriendly n source target)
                        | n == 0 = let player = (occupiedBy source b)
                                   in return $ (player `invades` target) b
                        | otherwise = return b
applyAction b (DefendField source target) =
                                   let player = (occupiedBy source b)
                                   in return $ updatePlayerPosition player target
                                             $ updatePlayerActions (>>Nothing) b -- terminate both actions.
applyAction b (ConquerNeutral n source target)
                        | n == 0 = let player = (occupiedBy source b)
                                   in return $ (player `invades` target) b
                        | otherwise = return b
;

invades :: Occupation -> Pos -> Board -> Board
invades p pos b = updatePlayerPosition p pos $ updateField pos p b

-- TODO test
-- For a direction to move and a Position,
-- this returns the position if moved in that direction.
-- For vertical movement, if both indices are ood or both are even, then
-- its the way to top. Else on has to go to bottom. The sum function
-- does that. The modulate wraps back the odices into legal ranges.
getAdjacentField :: Board -> Move -> Pos -> Pos
getAdjacentField b L (x,y) = modulate b (x-1,y)
getAdjacentField b R (x,y) = modulate b (x+1,y)
getAdjacentField b V (x,y)
                    | even (x+y) = modulate b(x,y-1)
                    | otherwise  = modulate b(x,y+1)
getAdjacentField _ Nil _  = error "Game Inconsistency. Make sure the game invariants are true."
;

-- makes sure that 0 <= x < width and 0 <= y < height
modulate :: Board -> Pos -> Pos
modulate b (x, y) = (x `mod` width b, y `mod` height b)


occupiedBy :: Pos -> Board -> Occupation
occupiedBy pos b = fields b ! pos


neutral, friendly, opposing :: Board -> Occupation -> Pos -> Bool
neutral b _ pos = occupiedBy pos b == N
friendly b p pos = occupiedBy pos b == p
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

isFinished :: Board -> Maybe Stats
isFinished b
            | finalTurns == turnCount b = case length pathA `compare` length pathB of
                                                LT -> return $ Stats (pName $ playerB b) pathB
                                                GT -> return $ Stats (pName $ playerB b) pathA
                                                EQ -> Nothing
            | otherwise = Nothing
        where
            pathA = (longestPath A b)
            pathB = (longestPath B b)
;



-- setters

updateField :: Pos -> Occupation -> Board -> Board
updateField pos p b = updateFields (Map.insert pos p) b

increaseTurnCount :: Board -> Board
increaseTurnCount b = b {turnCount = turnCount b + 1}

updateFields :: (Fields -> Fields) -> Board -> Board
updateFields f b = b {fields= f (fields b)}

updatePlayerActions :: (Maybe Action -> Maybe Action) -> Board -> Board
updatePlayerActions f b = b {playerA = p1', playerB = p2'}
                where
                    p1 = playerA b
                    p2 = playerB b
                    p1' = p1 {continuedAction= f (continuedAction p1)}
                    p2' = p2 {continuedAction= f (continuedAction p2)}
;

updatePlayerPosition :: Occupation -> Pos -> Board -> Board
updatePlayerPosition p pos b
                    | p == A = b {playerA = (playerA b) {pPos = pos}}
                    | p == B = b {playerB = (playerB b) {pPos = pos}}
                    | otherwise = error "Haskell impossible case 6"
;
