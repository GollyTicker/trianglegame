
module Gamedata where

import Data.Map.Strict (Map)

type Pos = (Int, Int)       -- zero-based indices

-- short-cut for the Map
type Fields = Map Pos Occupation

type Failable a = Either String a

data Board = Board {
                width :: Int,   -- width and height say the number of horizontal/vertical fields
                height :: Int,
                playerA :: Player,
                playerB :: Player,
                fields :: Fields, -- each field is either neutral or As field or Bs field.
                turnCount :: Int    -- counts the turns already moved on the board
            }   -- the show for this is defined in Trianglegame.hs
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
                    continuedAction :: Maybe Action
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
                deriving (Eq, Show)
;