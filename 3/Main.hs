{-# LANGUAGE CPP              #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Arrow
import           Data.Char                  (isDigit, ord)
import           Data.Function              (on)
import           Data.Functor               ((<&>))
import           Data.List                  (groupBy, sortOn)
import           Data.Maybe                 (fromJust)
import           Data.Tuple                 (swap)
import           Data.Void
import           Debug.Trace
import           System.Environment         (getArgs)
import           Test.Hspec                 (describe, hspec, it, shouldBe)
import           Text.Megaparsec
import qualified Text.Megaparsec.Char       as MP
import           Text.Megaparsec.Char.Lexer

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c || c == '.')

toInt :: Char -> Int
toInt c = ord c - ord '0'

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]

type V2 = (Int, Int)
type Symbol = V2
type Symbols = [Symbol]
type Number = (Int, Int, Int) -- x, y, number

------------
-- Part 1 --
------------

solve1 :: String -> Int
solve1 =
    sum . map (\(_, _, x) -> x)
        . (\(ss, ns) -> filter (hasAdjSymbol ss) ns)
        . (symbols &&& numbers)
        . lines

-- Get list of coordinates for all symbols
symbols :: [String] -> [V2]
symbols xs = go (1, 1) xs []
  where
    go :: V2 -> [String] -> Symbols -> Symbols
    go (i, j) []          acc = reverse acc -- End of input
    go (i, j) ([]:xs)     acc = go (i + 1, 1) xs acc -- New line
    go (i, j) ((c:cs):xs) acc =
        go (i, j + 1) (cs:xs) $ if isSymbol c then (i, j) : acc else acc

-- Get list of numbers - (row, column, number)
numbers :: [String] -> [Number]
numbers = concat . map go . enumerate
  where
    -- For each row index and row, get the nubmers on that row
    go :: (Int, String) -> [Number]
    go (i, s) =
        let -- f :: (Int, Char) -> [(Int, Int, Int)] -> [(Int, Int, Int)]
            f b (j, c) = if isDigit c then mergeOrInsert j (toInt c) b else b

            mergeOrInsert j d []           = [(i, j, d)]
            mergeOrInsert j d ((x,y,z):xs) =
                if j == y + 1
                    then (x, y, z `appendDigit` d) : xs
                    else (i, j, d) : xs
         in map (\xs -> (i, fst $ head xs, read @Int $ map snd xs))
                . filter (isDigit . snd . head )
                . groupBy (on (\a b -> isDigit a && isDigit b) snd)
                $ enumerate s

appendDigit :: Int -> Int -> Int
appendDigit x d = x * 10 + d

-- Get all adjacent coordinates
adj :: V2 -> [V2]
adj (x, y) =
    [(x + i, y + j) | i <- [-1, 1], j <- [-1, 1]] -- Corners
        ++ [(x + i, y) | i <- [-1, 1]] -- Left and right
        ++ [(x, y + j) | j <- [-1, 1]] -- Up and down

-- Check if a number has any symbols adjacent to it
hasAdjSymbol :: Symbols -> Number -> Bool
hasAdjSymbol ss (x, y, 0) = False
hasAdjSymbol ss (x, y, n) =
    any (`isAdj` (x, y)) ss || hasAdjSymbol ss (x, y + 1, n `div` 10)

isAdj :: V2 -> V2 -> Bool
isAdj u v = u `elem` adj v

------------
-- Part 2 --
------------

solve2 :: String -> Int
solve2 = undefined . lines

------------
-- Driver --
------------

main :: IO ()
main =
#ifdef test
    test
#else
#ifdef p1
    readFile "input" >>= putStrLn . show . solve1
#else
    readFile "input" >>= putStrLn . show . solve2
#endif
#endif

test :: IO ()
test = hspec $ do
    describe "part 1" $ do
        it "works with sample" $ do
            d <- readFile "sample_1.txt"
            solve1 d `shouldBe` 4361
        describe "symbols" $ do
            it "works" $ do
                d <- readFile "sample_1.txt"
                symbols (lines d) `shouldBe` [(2, 4), (4, 7), (5, 4), (6, 6), (9, 4), (9, 6)]
        describe "hasSymbolAround" $ do
            it "works" $ do
                d <- readFile "sample_1.txt"
                let ss = symbols (lines d)
                hasAdjSymbol ss (3, 3, 35) `shouldBe` True
                hasAdjSymbol ss (1, 1, 467) `shouldBe` True
    -- describe "part 2" $ do
    --     it "works with sample" $ do
    --         d <- readFile "sample_2.txt"
    --         solve2 d `shouldBe` 2286
