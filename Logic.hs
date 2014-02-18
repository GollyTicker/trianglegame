module Logic (
                mkBoard
              , longestPath
              , playMove
              , friendly
              , opposing
              , neutral
              , initPos
              , paths
              , updateField
              , adjacent
              , mkClearFields
              , opposingPos
            ) where

import Types -- everything , especially failing :: String -> Failable a

import Data.Map.Strict as Map (fromList, toList, filterWithKey, insert, (!))
import qualified Data.Map.Strict as Map (filter)    -- import Map.filter as qualified
import Control.Applicative ((<$>), (<*>))
import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.Set as Set (Set, fromList, toList, insert, delete)

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
The field both players are standing on are indeed theirs. (1)
Thats the players cannot move into the same (2)
field simultanously. (Thats means, that after all the currently runnings acitons are over,
that both players are an odd number of moves away from each other)
Also, the Set of Paths is never empty! (this can not happen, unless the fstInvariant is false.)

These invariants should be always true.
They are not mistakes by the player, but bugs in the logic.

Failure Messegaes with "Inconsistency" should NOT blubble up to the player!
There of the same type and should never arise!
-}
checkInvariants :: Board -> Failable Board
checkInvariants board
            | not fstInvariant = failing "Inconsistency. The fields the players are standing on are not theirs."
            | not sndInvariant = failing "Inconsistency. Players are even numbers of actions away form each other."
            | otherwise = return board
        where
            fstInvariant = occupiedBy (pPos $ playerA board) board == A && occupiedBy (pPos $ playerB board) board == B
            sndInvariant = True -- bothEvenOrBothOdd p1Potential p2Potential
            p1 = playerA board
            p2 = playerB board
            p1Potential = undefined -- sumPos (p1) + 
            p2Potential = undefined -- sumPos (p1) + 
            bothEvenOrBothOdd :: Int -> Int -> Bool
            bothEvenOrBothOdd a b = ((a `mod` 2) + (b `mod` 2)) `mod` 2 == 0
            sumPos :: Player -> Int
            sumPos plr = (\(a,b) -> a + b) (pPos plr)
                            
;

-- returns all the paths for a specific player(given as Occupation)
-- first, a new Board is made, where all the fields
-- with three neighbors are made neutral.
-- this destroyes paths with multiple branches.
-- Also, this has the effect, that all fields now have
-- atmost two adjacent freindly neighbors.
-- it starts with all singleton fields and
-- repeatedly merges connected paths
-- once all there are no more paths to merge,
-- the remaining set is returned
paths :: Occupation -> Board -> Set Path
paths player b = finalPaths
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
                                    let board1 = copyActionsIntoPlayers actionA actionB oldBoard
                                    board2 <- applyAction board1 actionA
                                    board3 <- applyAction board2 actionB
                                    let board4 = increaseTurnCount board3
                                        board5 = decreaseActionTurns board4
                                        rightsToMove = rightsFromBoard board5
                                    newBoard <- checkInvariants board5
                                    return (isFinished newBoard, newBoard, actionA, actionB, rightsToMove)
;

rightsFromBoard :: Board -> RightsToMove
rightsFromBoard b = case (actionOf A b, actionOf B b) of
                            (Just _, Just _) -> None
                            (Just _, Nothing) -> OnlyB
                            (Nothing, Just _) -> OnlyA
                            (Nothing, Nothing) -> Both
;

copyActionsIntoPlayers :: Action -> Action -> Board -> Board
copyActionsIntoPlayers act1 act2 b = 
                        b {playerA= (playerA b) {continuedAction=return act1}, playerB= (playerB b) {continuedAction=return act2}}
;

-- TODO test
toAction :: Move -> Occupation -> Board -> Failable Action
toAction mv p board
            | actionActive && nilMove = return previousAction
            | not actionActive && not nilMove = do 
                                                    (actionType, turnsToWait) <- typeAndTurns
                                                    let act = mkAction actionType turnsToWait from target
                                                    return (act)          -- redundant return, but left staying for clarity.
            | actionActive && not nilMove = failing "Game Inconsistency. Action active, but got a move command."
            | not actionActive && nilMove = failing "Game Inconsistency. Action inactive, but got a nilMove."
            | otherwise = error "Haskell impossible case 5"
            where
                from :: Pos
                from = pPos $ player
                target :: Pos
                target = getAdjacentField board mv from
                typeAndTurns :: Failable (String, Int)
                typeAndTurns
                    | neutral board p target = return ("ConquerNeutral", 0)
                    | friendly board p target = 
                                    if ((opponentOf p) `attacking` target) board
                                        then return ("DefendField", 0)
                                        else return ("VisitFriendly", 0)
                    | opposing board p target = 
                                    if (opponentOf p) `isCurrentlyStandingOn` target
                                        then failing $ (pName (player)) ++ " must not attack the opponent, if he/she is already defending the target."
                                        else return ("AttackOpponent", 2)
                    | otherwise = error "Haskell impossible case 3"
                player = playerByOcc p board
                nilMove = (mv == Nil)
                (previousAction, actionActive) = case continuedAction player of
                                                        Just prevAct -> (prevAct, True)
                                                        Nothing -> (undefined, False)
                isCurrentlyStandingOn :: Occupation -> Pos -> Bool
                plr `isCurrentlyStandingOn` pos = pPos (playerByOcc plr board) == pos
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

mkAction :: String -> Int -> Pos -> Pos -> Action
mkAction str wTurns from to
        | str == "AttackOpponent" = AttackOpponent wTurns from to
        | str == "VisitFriendly"  = VisitFriendly wTurns from to
        | str == "ConquerNeutral" = ConquerNeutral wTurns from to
        | str == "DefendField" = DefendField from to
        | otherwise = error "Haskell impossible case 4"
;

-- decrease the counter for the turns.
decreaseActionTurns :: Board -> Board
decreaseActionTurns = updatePlayerActions (>>=f)
        where
            f :: Action -> Maybe Action
            f act
                | waitTurns act == 0 = Nothing
                | otherwise = return (act {waitTurns=waitTurns act-1})
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
finalTurns = 42

isFinished :: Board -> Maybe Stats
isFinished b
            | finalTurns < turnCount b = case length pathA `compare` length pathB of
                                                LT -> return $ Stats (pName $ playerB b) pathB
                                                GT -> return $ Stats (pName $ playerA b) pathA
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

