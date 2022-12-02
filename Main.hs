{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bitraversable (Bitraversable (bitraverse))
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import Data.List (sort)
import Data.Monoid (Sum (..))
import Data.Ord (Down (..))
import Data.Semigroup (Max (..))

type Day = BS.ByteString -> IO ()

doParse :: Either String c -> c
doParse = either error id

main :: IO ()
main = traverse_ (\(f, i) -> f =<< BS.readFile ("day" <> show i <> ".txt")) $ zip days [1 ..]
  where
    days :: [Day]
    days = [day1, day2]

day1 :: Day
day1 = print . (\x -> (maxOfSum x, topThreeSum x)) . parse
  where
    parse =
      doParse
        . A.parseOnly
          (A.sepBy' (A.sepBy' ((round @_ @Int) <$> A.scientific) A.space) A.space)

    maxOfSum =
      getMax
        . foldMap (Max . getSum . foldMap Sum)

    topThreeSum =
      getSum
        . foldMap (Sum . getDown)
        . take 3
        . sort
        . fmap (Down . getSum . foldMap Sum)

data Day2Shape
  = Rock
  | Paper
  | Scissor

day2ShapeScore :: Day2Shape -> Int
day2ShapeScore Rock = 1
day2ShapeScore Paper = 2
day2ShapeScore Scissor = 3

data Day2RoundOutcome
  = Draw
  | Win
  | Lose

day2Score :: Day2RoundOutcome -> Int
day2Score Lose = 0
day2Score Draw = 3
day2Score Win = 6

day2Round :: Day2Shape -> Day2Shape -> Day2RoundOutcome
day2Round Rock Rock = Draw
day2Round Rock Paper = Win
day2Round Rock Scissor = Lose
day2Round Paper Paper = Draw
day2Round Paper Rock = Lose
day2Round Paper Scissor = Win
day2Round Scissor Scissor = Draw
day2Round Scissor Rock = Win
day2Round Scissor Paper = Lose

day2ShapeRecover :: Day2Shape -> Day2RoundOutcome -> Day2Shape
day2ShapeRecover Rock Lose = Scissor
day2ShapeRecover Rock Win = Paper
day2ShapeRecover Paper Lose = Rock
day2ShapeRecover Paper Win = Scissor
day2ShapeRecover Scissor Lose = Paper
day2ShapeRecover Scissor Win = Rock
day2ShapeRecover x Draw = x

day2 :: Day
day2 = print . sequence [totalScore . parse, totalScoreRecovered . parseRecover]
  where
    parseRPS 'A' = Rock
    parseRPS 'B' = Paper
    parseRPS 'C' = Scissor
    parseRPS 'X' = Rock
    parseRPS 'Y' = Paper
    parseRPS 'Z' = Scissor
    parseRPS _ = error "impossible"
    parseRoundShape = parseRPS <$> A.satisfy (`elem` ['A', 'B', 'C', 'X', 'Y', 'Z'])

    parseRO 'X' = Lose
    parseRO 'Y' = Draw
    parseRO 'Z' = Win
    parseRO _ = error "impossible"
    parseRoundOutcome = parseRO <$> A.satisfy (`elem` ['X', 'Y', 'Z'])

    parse =
      doParse
        . A.parseOnly
          (A.sepBy' ((,) <$> parseRoundShape <*> (A.space *> parseRoundShape)) A.space)

    parseRecover =
      doParse
        . A.parseOnly
          (A.sepBy' ((,) <$> parseRoundShape <*> (A.space *> parseRoundOutcome)) A.space)

    computeScore (oponnentShape, shape) =
      day2Score (day2Round oponnentShape shape) + day2ShapeScore shape

    totalScore = getSum . foldMap (Sum . computeScore)

    computeScoreRecover (oponnentShape, outcome) =
      day2Score outcome + day2ShapeScore (day2ShapeRecover oponnentShape outcome)

    totalScoreRecovered =
      getSum . foldMap (Sum . computeScoreRecover)
