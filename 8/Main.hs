{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Arrow
import qualified Data.Bifunctor             as B (first)
import           Data.Char                  (ord)
import           Data.Either                (fromRight)
import           Data.Function              ((&))
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Data.Void                  (Void)
import           Debug.Trace
import           System.Environment         (getArgs)
import           Test.Hspec
import qualified Text.Megaparsec            as MP
import           Text.Megaparsec            (Parsec)
import qualified Text.Megaparsec.Char       as MP
import qualified Text.Megaparsec.Char.Lexer as MP
import qualified Text.Megaparsec.Debug      as MP

data Step = R | L deriving (Show)
type Steps = [Step]

type Vertex = String
type Edge = (String, String)
type Graph = Map Vertex (Vertex, Vertex)

type AdjMatrix = Vector (Vertex, Vertex)

-- toMatrix :: Graph -> AdjMatrix
-- toMatrix g = V.generate 26 (\i -> fromJust (Map.lookup i g))

------------
-- Part 1 --
------------

dst :: Int
dst = ord 'Z' - ord 'A'

solve1 :: String -> Int
solve1 =
    length
        . takeWhile (/= "ZZZ")
        . (\(d, g) -> scanl (step g) "AAA" (cycle d))
        . either (error . show) id
        . parse

step :: Graph -> Vertex -> Step -> Vertex
step g v s =
    case s of
        L -> l
        R -> r
  where
    (l, r) = fromJust $ Map.lookup v g

-------------------
-- Parsing input --
-------------------

type Parser = Parsec Void String
type Err = MP.ParseErrorBundle String Void

parse :: String -> Either String (Steps, Graph)
parse s =
    let eresult = MP.runParser ((,) <$> pSteps <* MP.newline <*> pGraph) "" s
     in B.first MP.errorBundlePretty eresult

-- Steps are a sequence of R or L characters
pSteps :: Parser Steps
pSteps = MP.some pStep <* MP.newline
  where
    pStep :: Parser Step
    pStep = (\c -> if c == 'R' then R else L) <$> MP.upperChar

pGraph :: Parser Graph
pGraph = post <$> MP.many (pEdges <* MP.newline)
  where
    pEdges :: Parser (Vertex, (Vertex, Vertex))
    pEdges = (,) <$> (pVertex <* MP.hspace <* equal <* MP.hspace) <*> pDsts

    pDsts :: Parser (Vertex, Vertex)
    pDsts = tuple pVertex pVertex

    pVertex :: Parser Vertex
    pVertex = MP.count 3 MP.upperChar

    toInt :: Char -> Int
    toInt c = ord c - ord 'A'

    post = Map.unions . map (uncurry Map.singleton)

equal = MP.char '='
comma = MP.char ','
parens = MP.between (MP.char '(') (MP.char ')')

tuple :: Parser a -> Parser b -> Parser (a, b)
tuple l r = parens ((,) <$> l <* comma <* MP.hspace <*> r)


------------
-- Part 2 --
------------

-- solve2 :: String -> _
solve2 = undefined

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
        it "works with sample 1" $ do
            d <- readFile "sample_1.txt"
            solve1 d `shouldBe` 2
        it "works with  sample 2" $ do
            d <- readFile "sample_2.txt"
            solve1 d `shouldBe` 6
    -- describe "part 2" $ do
    --     it "works with sample" $ do
    --         d <- readFile "sample_2.txt"
    --         solve2 d `shouldBe` _
