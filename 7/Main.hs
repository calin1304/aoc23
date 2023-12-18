{-# LANGUAGE CPP              #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Arrow
import           Data.Char          (ord)
import           Data.Function      (on)
import           Data.List
import           Data.Ord
import           Debug.Trace
import           GHC.Prim           (coerce)
import           System.Environment (getArgs)
import           Test.Hspec

type Hand = [Int]
type Bid = Int

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]

------------
-- Part 1 --
------------

solve1 :: String -> Int
solve1 =
    sum . map (uncurry (*) . (id *** snd))
        . enumerate
        . sortBy (on foo fst)
        . map parse
        . lines

parse :: String -> (Hand, Bid)
parse = words >>> \case
    [x, y] -> (hand x, read y)
    _      -> error "why am I doing this to myself?"
  where
    hand :: String -> Hand
    hand = map toInt

    toInt :: Char -> Int
    toInt = \case
        'T' -> 10
        'J' -> 11
        'Q' -> 12
        'K' -> 13
        'A' -> 14
        x   -> ord x - ord '0'

foo :: Hand -> Hand -> Ordering
foo x y =
    let f :: Hand -> [Int] -- from hand to groups by length
        f = coerce . sort @(Down Int) . coerce . map length . group . sort
     in compare (f x) (f y) <> compare x y
------------
-- Part 2 --
------------

-- solve2 :: String -> _
-- solve2 = undefined

------------
-- Driver --
------------

main :: IO ()
main =
#if p1
    readFile "input" >>= putStrLn . show . solve1
#elif p2
    readFile "input" >>= putStrLn . show . solve2
#else
    test
#endif

test :: IO ()
test = hspec $ do
    describe "part 1" $ do
        it "works with sample" $ do
            d <- readFile "sample_1.txt"
            solve1 d `shouldBe` 6440
    -- describe "part 2" $ do
    --     it "works with sample" $ do
    --         d <- readFile "sample_2.txt"
    --         solve2 d `shouldBe` _
