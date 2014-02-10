
import Data.Map (Map)

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

main :: IO ()
main = do
        putStrLn ">> Running HUnit tests..."
        counts <- runTestTT $ TestList unitsProblemTests
        putStrLn ">> Finished HUnit Tests."
        if failures counts /= 0
            then putStrLn ">> Tests failed."
            else putStrLn ">> All ok."
;

unitsProblemTests = [
                       "testBoard" ~:  testBoard
                    ]
;

testBoard = mapForTest [
                        
                      ]
;



-- =====================================================================

data Board = Board {
            width :: Int,
            height :: Int,
            playerA :: Player,
            playerB :: Player),
            fields :: Map (Int, Int) Occupacy
            }
;

data Player = Player Pos -- more ....

data Occupacy = A | B | Neutral




