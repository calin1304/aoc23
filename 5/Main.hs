{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Arrow              hiding (first)
import           Data.Bifunctor             (first)
import           Data.Function              ((&))
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Void                  (Void)
import           Debug.Trace
import           System.Environment         (getArgs)
import           Test.Hspec
import           Test.Hspec.Megaparsec
import qualified Text.Megaparsec            as MP
import           Text.Megaparsec            (Parsec, (<?>))
import qualified Text.Megaparsec.Char       as MP
import qualified Text.Megaparsec.Char.Lexer as MP hiding (space)
import qualified Text.Megaparsec.Debug      as MP

data Mapping = Mapping
    { source      :: String
    , destination :: String
    , tbl         :: Map Int Int
    } deriving (Show, Eq)

type Parser = Parsec Void String
type Err = MP.ParseErrorBundle String Void

parse p = first MP.errorBundlePretty . MP.parse p ""

parseInput :: String -> Either String ([Int], Int -> Int)
parseInput = parse p
  where
    p :: Parser ([Int], Int -> Int)
    p =
        (,)
            <$> (pSeeds <* MP.newline <* MP.newline)
            <*> (fmap (foldl1 (>>>)) $ pMapping' `MP.sepBy1` MP.newline)

-- Sequence of seed numbers
pSeeds :: Parser [Int]
pSeeds = MP.string "seeds: " *> decimals
  where
    -- A sequence of decimals
    decimals :: Parser [Int]
    decimals = MP.decimal `MP.sepBy1` MP.hspace

type F = Int-> (Int -> Int) -> Int

comb :: F -> F -> F
comb f g = \x h -> f x (\y -> g y h)

pMapping' :: Parser (Int -> Int)
pMapping' = (\f x -> f x id) . foldl1 comb <$> (pTitle *> MP.newline *> MP.some pEntry)
  where
    pTitle :: Parser (String, String)
    pTitle =
        (,)
            <$> (word <* MP.string "-to-")
            <*> (word <* MP.string " map:")

    pEntry :: Parser (Int -> (Int -> Int) -> Int)
    pEntry =
        (\y x n a cont -> if a >= x && a < x + n then a - x + y else cont a)
            <$> (MP.decimal <* MP.hspace)
            <*> (MP.decimal <* MP.hspace)
            <*> (MP.decimal <* MP.optional MP.eol)

word :: Parser String
word = MP.some MP.lowerChar

------------
-- Part 1 --
------------

solve1 :: String -> Int
solve1 s = case uncurry go <$> parseInput s of
    Left e -> error e
    Right r -> r
  where
    go :: [Int] -> (Int -> Int) -> Int
    go seeds mapping = minimum $ map mapping seeds


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
        describe "solve" $ do
            it "works with sample" $ do
                d <- readFile "sample_1.txt"
                solve1 d `shouldBe` 35
            it "works with sample light-to-temperature" $ do
                let inp = unlines
                        [ "seeds: 74\n"
                        , "light-to-temperature map:"
                        , "45 77 23"
                        , "81 45 19"
                        , "68 64 13"
                        ]
                solve1 inp `shouldBe` 78
            it "works with sample temperature-to-humidity" $ do
                let inp = unlines
                        [ "seeds: 78\n"
                        , "temperature-to-humidity map:"
                        , "0 69 1"
                        , "1 0 69"
                        ]
                solve1 inp `shouldBe` 78
            it "works with sample humidity-to-location" $ do
                let inp = unlines
                        [ "seeds: 78\n"
                        , "humidity-to-location map:"
                        , "60 56 37"
                        , "56 93 4"
                        ]
                solve1 inp `shouldBe` 82
            it "works" $ do
                let inp = unlines
                        [ "seeds: 0\n"
                        , "a-to-b map:"
                        , "1 0 2\n"
                        , "b-to-c map:"
                        , "100 0 2"
                        ]
                solve1 inp `shouldBe` 101
        describe "parser" $ do
            describe "seeds" $ do
                it "works" $ do
                    let inp = "seeds: 79 14 55 13"
                    MP.parse pSeeds "" inp `shouldParse` [79, 14, 55, 13]
    -- describe "part 2" $ do
    --     it "works with sample" $ do
    --         d <- readFile "sample_2.txt"
    --         solve2 d `shouldBe` _
