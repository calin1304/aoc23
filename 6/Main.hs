{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Arrow
import           Debug.Trace
import           System.Environment (getArgs)
import           Test.Hspec

------------
-- Part 1 --
------------

solve1 :: String -> Int
solve1 = product . map go . parse
  where
    go :: (Int, Int) -> Int
    go (t, d) = ccc $ sol (- 1, fromIntegral t, - (fromIntegral d))

parse :: String -> [(Int, Int)]
parse s =
    case lines s of
        [t, d] ->
            let f = map read . words . tail . snd . break (== ':')
             in zip (f t) (f d)
        _ -> error "lazyyyy"

delta :: (Float, Float, Float) -> Float
delta (a, b, c) = b * b - 4 * a * c

sol :: (Float, Float, Float) -> [Float]
sol (a, b, c) = [go (+), go (-)]
  where
    go op = ((- b) `op` sqrt (delta (a, b, c))) / (2 * a)

ccc :: [Float] -> Int
ccc sol =
    case sol of
        [x1, x2] -> ceiling (x2 - 1) - floor (x1 + 1) + 1
        _ -> error "You suck"

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
#ifdef TEST
    test
#else
    readFile "input" >>= putStrLn . show . solve1
#endif

test :: IO ()
test = hspec $ do
    describe "part 1" $ do
        it "works with sample" $ do
            d <- readFile "sample_1.txt"
            solve1 d `shouldBe` 288
    -- describe "part 2" $ do
    --     it "works with sample" $ do
    --         d <- readFile "sample_2.txt"
    --         solve2 d `shouldBe` _
