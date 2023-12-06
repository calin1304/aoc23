{-# LANGUAGE CPP              #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Arrow
import           Data.Function
import           Debug.Trace
import           System.Environment (getArgs)
import           Test.Hspec

import           Data.Char          (isDigit)
import           Data.List          (isPrefixOf)
import           Data.Maybe         (fromJust)
import           Text.Regex.TDFA


solve1 :: String -> Int
solve1 = sum . map (go . filter isDigit) . lines
  where
    go :: String -> Int
    go = read . (uncurry (:)) . (head &&& (pure . last))

solve2 :: String -> Int
solve2 = sum . map go . lines
  where
    go :: String -> Int
    go s =
        (uncurry (+) . first (*10))
        . (f *** f)
        . (head &&& last)
        $ g s []

    f :: String -> Int
    f s = if isDigit (head s) then read s else fromJust $ lookup s tbl
      where
        tbl = zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1..9]

names :: [String]
names = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    ++ [show d | d <- [1..9]]

g :: String -> [String] -> [String]
g "" acc = reverse acc
g s@(c:cs) acc =
    g cs $ case filter (`isPrefixOf` s) names of
        (x:xs) -> (x : acc)
        _      -> acc

------------
-- Driver --
------------

main :: IO ()
main =
#ifdef TEST
    test
#else
    readFile "input" >>= putStrLn . show . solve2
#endif

test :: IO ()
test = hspec $ do
    describe "solve1" $ do
        it "works on sample data" $ do
            d <- readFile "sample_1-1.txt"
            solve1 d `shouldBe` 142
    describe "day 1 part 2" $ do
        it "works on sample data" $ do
            d <- readFile "sample_1-2.txt"
            solve2 d `shouldBe` 281
        it "works on edge case" $ do
            -- last digit `eight` overlaps with `one`
            let d = "mnbrf3fourfpbrdgltf2xbmbmrbjltdxbklsixoneightq"
            solve2 d `shouldBe` 38
