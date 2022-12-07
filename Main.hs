{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative (Alternative (..))
import Control.Lens
import Control.Monad (join, replicateM)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor.Join (Join (..))
import Data.Bitraversable (Bitraversable (..))
import qualified Data.ByteString.Char8 as BS
import Data.Char (isUpper, ord)
import Data.Foldable (fold, foldMap', traverse_)
import Data.Functor (($>))
import qualified Data.IntSet as S
import Data.Ix (inRange)
import Data.List (sort, transpose)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid (Endo (..), Sum (..))
import Data.Ord (Down (..))
import Data.Semigroup (Max (..))
import Debug.Trace (traceShow, traceShowId)

type Day = BS.ByteString -> IO ()

parseOrFail :: A.Parser a -> BS.ByteString -> a
parseOrFail x = either error id . A.parseOnly x

main :: IO ()
main = traverse_ (\(f, i) -> f =<< BS.readFile ("day" <> show i <> ".txt")) $ zip days [1 ..]
  where
    days :: [Day]
    days = [day1, day2, day3, day4, day5, day6]

day1 :: Day
day1 = print . (\x -> (maxOfSum x, topThreeSum x)) . parse
  where
    parse =
      parseOrFail $
        A.sepBy' (A.sepBy' (A.decimal @Int) A.space) A.space

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
  deriving (Show)

day2ShapeScore :: Day2Shape -> Int
day2ShapeScore Rock = 1
day2ShapeScore Paper = 2
day2ShapeScore Scissor = 3

data Day2RoundOutcome
  = Draw
  | Win
  | Lose
  deriving (Show)

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
day2 = print . sequenceA [totalScore . parse, totalScoreRecovered . parseRecover]
  where
    parseEntry c e = A.char c $> e
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
day3 = print . sequenceA [sumOfAllCommon . parseBags, sumOfAllCommonInGroups . parseGroupBags]
  where
    parseBag = A.many' A.letter_ascii

    parseBags =
      parseOrFail $ A.sepBy' parseBag A.space

    parseGroupBags =
      parseOrFail $
        A.sepBy'
          ( (,,)
              <$> parseBag
              <*> (A.space *> parseBag)
              <*> (A.space *> parseBag)
          )
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
day4 = print . sequenceA [nbOf superset, nbOf overlap] . parsePairs
  where
    parseSection =
      (,)
        <$> A.decimal @Int
        <*> (A.char '-' *> A.decimal @Int)

    parsePairs =
      parseOrFail $
        A.sepBy'
          ( (,)
              <$> parseSection
              <*> (A.char ',' *> parseSection)
          )
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

type Stack a = [a]

day5 :: Day
day5 =
  print
    . fmap (mapMaybe headMay)
    . sequenceA
      [ execute applyMovements,
        execute applyMovementsBulk
      ]
    . parseOrFail parseStack
  where
    parseStackEmpty = A.string "   "

    parseStackElem =
      A.char '[' *> A.letter_ascii <* A.char ']'

    parseStackElems =
      A.sepBy1'
        (Just <$> parseStackElem <|> (Nothing <$ parseStackEmpty))
        (A.satisfy (A.isHorizontalSpace . fromIntegral . ord))

    parseMovement =
      (,,)
        <$> (A.string "move" *> A.skipSpace *> A.decimal @Int <* A.skipSpace)
        <*> (A.string "from" *> A.skipSpace *> A.decimal @Int <* A.skipSpace)
        <*> (A.string "to" *> A.skipSpace *> A.decimal @Int)

    parseStack =
      (,)
        <$> A.sepBy1' parseStackElems A.endOfLine
        <* A.endOfLine
        <*> ( replicateM 2 (A.takeTill (A.isEndOfLine . fromIntegral . ord) *> A.endOfLine)
                *> (reverse <$> A.sepBy' parseMovement A.endOfLine)
            )

    headMay (x : _) = Just x
    headMay _ = Nothing

    applyMovement :: Int -> Int -> [Stack Char] -> [Stack Char]
    applyMovement from to s =
      let (x : _, s') = s & element (from - 1) <<%~ tail
       in s' & element (to - 1) %~ (x :)

    applyMovements :: [(Int, Int, Int)] -> [Stack Char] -> [Stack Char]
    applyMovements =
      appEndo
        . foldMap (\(n, from, to) -> foldMap Endo $ replicate n (applyMovement from to))

    applyMovementsBulk :: [(Int, Int, Int)] -> [Stack Char] -> [Stack Char]
    applyMovementsBulk =
      appEndo
        . foldMap
          ( \(n, from, to) ->
              Endo $ \s ->
                let (xs, s') = s & element (from - 1) <<%~ drop n
                 in s' & element (to - 1) %~ (take n xs <>)
          )

    execute appMoves (stacks, moves) =
      appMoves moves (catMaybes <$> transpose stacks)

day6 :: Day
day6 = print . sequenceA [prefix 4 0, prefix 14 0] . parseStream
  where
    parseStream = parseOrFail $ A.many' (ord <$> A.anyChar)

    prefix _ _ [] = Nothing
    prefix k i xs
      | S.size (S.fromList $ take k xs) == k = Just $ i + k
      | otherwise = prefix k (i + 1) $ drop 1 xs
