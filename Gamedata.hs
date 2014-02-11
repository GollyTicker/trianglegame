
module Gamedata where

import Data.Map.Strict (Map)

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
            }   -- the show for this is defined in Trianglegame.hs
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