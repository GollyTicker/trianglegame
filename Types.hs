
module Types where

import Data.Map.Strict (Map)

type Pos = (Int, Int)       -- zero-based indices

-- short-cut for the Map
type Fields = Map Pos Occupation

type Path = [Pos]

type Failable a = Either String a

data Board = Board {
                width :: Int,   -- width and height say the number of horizontal/vertical fields
                height :: Int,
                playerA :: Player,
                playerB :: Player,
                fields :: Fields, -- each field is either neutral or As field or Bs field.
                turnCount :: Int    -- counts the turns already moved on the board
            }  deriving Show
;

-- each field is either neutral or As field or Bs field.
data Occupation = A | B | N deriving (Show, Eq)

-- currently a Player has a Position and a name
-- the action taken in the previous round is also saved,
-- to see whether its an action that has to be continued
-- If its Nothing, then one can make an action. If its an action
-- then one has to continue that action.
data Player = Player {
                    pPos :: Pos,
                    pName :: String,
                    continuedAction :: Maybe Action,
                    pOccupation :: Occupation   -- this is never Neutral
              } deriving (Show, Eq)
;


data Stats = Stats {
                    winner :: String,
                    winningPath :: Path
                }
;

data RightsToMove = Both | OnlyA | OnlyB | None deriving (Show, Eq)

-- Seen from the quadratic representation, a move goes either to the left or to the right or vertically.
-- whether it goes to the top or bottom is dependent form the actual field.
data Move = L | R | V | Nil deriving (Show, Eq, Read)
-- the Nil move shoudlt be used by the players. It's just for an internal represenatation.

data Action =     AttackOpponent { 
                        waitTurns :: Int,
                        fromField :: Pos,
                        toField :: Pos
                        }
                | VisitFriendly {
                        waitTurns :: Int,
                        fromField :: Pos,
                        toField :: Pos
                        }
                | ConquerNeutral {
                        waitTurns :: Int,
                        fromField :: Pos,
                        toField :: Pos
                        }
                | DefendField {
                        fromField :: Pos,
                        toField :: Pos
                        }
                deriving (Eq, Show)
;

-- ==================================================================================0

-- ======================================================================

-- can't use fail here, because its an error. I just want normal Left error though.
failing :: String -> Failable a
failing str = Left str

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

-- abcd is a function which needs to use multiple functions that can failing.
abcd :: Integer -> Failable Integer
abcd n = do 
            x <- firstUnsafeFunction n
            y <- secondUnsafeFunction x
            if y == 4
                then failing "I don't like four."
                else return y
;

-- firstUnsafeFunction and secondUnsafeFunction are functions which can failing with an error Message
firstUnsafeFunction, secondUnsafeFunction :: Integral a => a -> Failable a
firstUnsafeFunction n
    | n <= 5 = return n
    | otherwise = failing "its too large"
;

secondUnsafeFunction n
    | n >= 1 = return n
    | otherwise = failing "sorry, its too small"
;
