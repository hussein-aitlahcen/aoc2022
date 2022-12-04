{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BS
import Data.Bitraversable (Bitraversable(..))
import Data.Foldable (traverse_, foldMap')
import Data.List (sort)
import Data.Monoid (Sum (..))
import Data.Ord (Down (..))
import Data.Semigroup (Max (..))
import Control.Applicative (Alternative(..))
import Debug.Trace (traceShowId)
import qualified Data.IntSet as S
import Data.Char (isUpper, ord)
import Data.Bifunctor.Join (Join(..))

type Day = BS.ByteString -> IO ()

parseOrFail :: A.Parser a -> BS.ByteString -> a
parseOrFail x = either error id . A.parseOnly x

main :: IO ()
main = traverse_ (\(f, i) -> f =<< BS.readFile ("day" <> show i <> ".txt")) $ zip days [1 ..]
  where
    days :: [Day]
    days = [day1, day2, day3, day4]

day1 :: Day
day1 = print . (\x -> (maxOfSum x, topThreeSum x)) . parse
  where
    parse =
      parseOrFail $ 
          A.sepBy' (A.sepBy' ((round @_ @Int) <$> A.scientific) A.space) A.space

    maxOfSum =
      getMax
        . foldMap' (Max . getSum . foldMap' Sum)

    topThreeSum =
      getSum
        . foldMap' getDown
        . take 3
        . sort
        . fmap (Down . foldMap' Sum)

data Day2Shape
  = Rock
  | Paper
  | Scissor
  deriving Show

day2ShapeScore :: Day2Shape -> Int
day2ShapeScore Rock = 1
day2ShapeScore Paper = 2
day2ShapeScore Scissor = 3

data Day2RoundOutcome
  = Draw
  | Win
  | Lose
  deriving Show

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
    parseEntry c e = A.char c *> pure e
    oponnentRock = parseEntry 'A' Rock
    oponnentPaper = parseEntry 'B' Paper
    oponnentScissor = parseEntry 'C' Scissor

    rock = parseEntry 'X' Rock
    paper = parseEntry 'Y' Paper
    scissor = parseEntry 'Z' Scissor

    parseRoundShape = 
      oponnentRock 
        <|> oponnentPaper 
        <|> oponnentScissor 
        <|> rock 
        <|> paper 
        <|> scissor

    lose = parseEntry 'X' Lose
    draw = parseEntry 'Y' Draw
    win = parseEntry 'Z' Win

    parseRoundOutcome = lose <|> draw <|> win

    parse =
      parseOrFail $
          A.sepBy' ((,) <$> parseRoundShape <*> (A.space *> parseRoundShape)) A.space

    parseRecover =
      parseOrFail $
          A.sepBy' ((,) <$> parseRoundShape <*> (A.space *> parseRoundOutcome)) A.space

    computeScore (oponnentShape, shape) =
      day2Score (day2Round oponnentShape shape) + day2ShapeScore shape

    totalScore = getSum . foldMap' (Sum . computeScore)

    computeScoreRecover (oponnentShape, outcome) =
      day2Score outcome + day2ShapeScore (day2ShapeRecover oponnentShape outcome)

    totalScoreRecovered =
      getSum . foldMap' (Sum . computeScoreRecover)


day3 :: Day
day3 = print . sequence [sumOfAllCommon . parseBags, sumOfAllCommonInGroups . parseGroupBags]
  where
    parseBag = A.many' A.letter_ascii

    parseBags = 
      parseOrFail $ A.sepBy' parseBag A.space

    parseGroupBags =
      parseOrFail $
        A.sepBy'
          ((,,) 
            <$> parseBag 
            <*> (A.space *> parseBag) 
            <*> (A.space *> parseBag)) 
          A.space

    priority x 
      | isUpper x = ord x - ord 'A' + 27
      | otherwise = ord x - ord 'a' + 1

    transform = S.fromList . fmap priority

    compartify x = 
      runJoin . fmap transform . Join $ splitAt (length x `div` 2) x

    commonInBag = uncurry S.intersection . compartify

    sumOfCommon = getSum . foldMap' Sum . S.toList

    sumOfAllCommon = getSum . foldMap' (Sum . sumOfCommon . commonInBag)

    commonInGroup (x, y, z) =
       transform x `S.intersection` transform y `S.intersection` transform z

    sumOfAllCommonInGroups = 
      getSum . foldMap' (foldMap' Sum . S.toList . commonInGroup)

day4 :: Day
day4 = print . sequence [nbOf superset, nbOf overlap] . parsePairs
  where
    parseSection = 
      (,) 
        <$> (round @_ @Int <$> A.scientific) 
        <*> (A.char '-' *> (round @_ @Int <$> A.scientific))

    parsePairs = 
      parseOrFail $
        A.sepBy'
          ((,) 
            <$> parseSection 
            <*> (A.char ',' *> parseSection)) 
          A.space

    sectionRange (x, y) = S.fromList [x .. y]

    nbOf x = 
      length 
        . filter x 
        . fmap (runJoin . fmap sectionRange . Join) 

    superset (x, y) =
      let s = S.size $ x `S.intersection` y
      in s == S.size x || s == S.size y

    overlap (x, y) =
      S.size (x `S.intersection` y) > 0
