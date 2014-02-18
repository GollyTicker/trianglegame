module Test (main) where

import Trianglegame hiding (main) -- everything (write Trianglegame.main to access the game)

import Test.HUnit (Assertion, runTestTT, assertEqual, failures, (~:))
import Data.Set as Set (fromList)
import Data.Map.Strict as Map (size)
import Control.Applicative ((<$>), (<*>))

-- runs all tests in the console
main :: IO ()
main = do
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
               
               {- TODO : more tests-} -- this is working already prolly
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
              
              -- adjacency tests
              ,assertEqual "adjacent a b <-> adjacent b a" True
                        $ all (\(a, b) -> if (b `elem` adjacent a initialBoard) then (b `elem` adjacent a initialBoard) else True)
                        $ (\ps -> (,) <$> ps <*> ps)
                        $ [(x,y)|x <- [0..6-1], y <- [0..4-1]]
              
              -- now coming tests of progressed games
              ,assertEqual "simple win" (Right $ [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0)])
                                        $ 
                                        (progressedGame $ zip (replicate 5 R) (repeat V))
                                        >>= (\b -> return $ longestPath A b)
              
              ,assertEqual "baseForTests"
                                (Right $ unlines [
                                        "",
                                        "    --    --    --",
                                        " 00 10 20 30 40 50",
                                        " A  A  N  Bb N  N ",
                                        " --    --    --   ",
                                        " 01 11 21 31 41 51",
                                        " A  Aa B  B  N  N ",
                                        "    --    --    --",
                                        " 02 12 22 32 42 52",
                                        " N  N  B  B  N  N ",
                                        " --    --    --   ",
                                        " 03 13 23 33 43 53",
                                        " N  N  N  N  N  N ",
                                        "    --    --    --"
                                    ])
                                $ (fmap prettyShow baseBoard)
              
              ,assertEqual "simpleAttack"
                                (Right $ unlines [
                                        "",
                                        "    --    --    --",
                                        " 00 10 20 30 40 50",
                                        " A  A  N  B  N  N ",
                                        " --    --    --   ",
                                        " 01 11 21 31 41 51",
                                        " A  A  Aa Bb N  N ",
                                        "    --    --    --",
                                        " 02 12 22 32 42 52",
                                        " N  N  B  B  N  N ",
                                        " --    --    --   ",
                                        " 03 13 23 33 43 53",
                                        " N  N  N  N  N  N ",
                                        "    --    --    --"
                                    ])
                                $ (fmap prettyShow $ [(R,V),(Nil,V),(Nil,V)] `playedOn` baseBoard)
              
              ,assertEqual "cannot attack the other player directly"
                                (Left "Q must not attack the opponent, if he/she is already defending the target.")
                                $ (fmap prettyShow $ [(R,V),(Nil,V),(Nil,V),(R,V)] `playedOn` baseBoard)
              
              ,assertEqual "blocking an Attack"
                                (Right $ unlines [
                                        "",
                                        "    --    --    --",
                                        " 00 10 20 30 40 50",
                                        " A  A  N  B  N  N ",
                                        " --    --    --   ",
                                        " 01 11 21 31 41 51",
                                        " A  Aa Bb B  N  N ",
                                        "    --    --    --",
                                        " 02 12 22 32 42 52",
                                        " N  N  B  B  N  N ",
                                        " --    --    --   ",
                                        " 03 13 23 33 43 53",
                                        " N  N  N  N  N  N ",
                                        "    --    --    --"
                                    ])
                                $ (fmap prettyShow $ [(R,V),(Nil,L)] `playedOn` baseBoard)
              
              ,assertEqual "blocking an Attack 2. actions are stopped."
                                (Right (Nothing, Nothing))
                                $ ([(R,V),(Nil,L)] `playedOn` baseBoard >>= (\b -> return (continuedAction $ playerA b, continuedAction $ playerB b)))
              
              ,assertEqual "continue after blocking an attack."
                                (Right $ unlines [
                                        "",
                                        "    --    --    --",
                                        " 00 10 20 30 40 50",
                                        " A  A  N  B  N  N ",
                                        " --    --    --   ",
                                        " 01 11 21 31 41 51",
                                        " Aa A  B  B  N  N ",
                                        "    --    --    --",
                                        " 02 12 22 32 42 52",
                                        " N  N  Bb B  N  N ",
                                        " --    --    --   ",
                                        " 03 13 23 33 43 53",
                                        " N  N  N  N  N  N ",
                                        "    --    --    --"
                                    ])
                                $ (fmap prettyShow $ [(R,V),(Nil,L),(L,V)] `playedOn` baseBoard)
            ]
;

initialBoard :: Board
initialBoard = mkBoard 6 4 "Q" "W"

baseBoard :: Failable Board
baseBoard = progressedGame [(R,L),(V,V),(L,R),(R,V)]

progressedGame :: [(Move, Move)] -> Failable Board
progressedGame xs = xs `playedOn` (return initialBoard)
;

playedOn :: [(Move, Move)] -> Failable Board -> Failable Board
xs `playedOn` board = foldl (\b mv -> b >>= playMove' mv) board xs

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

