
{-|
Module      : Mastermind
Description : Mastermind game solver
Maintainer  : Pedro Calixto <pgcalixto@gmail.com>

This module is responsible for exposing the functions used
to solve the Mastermind game which is played by communicating
with an external source which serves as a master.
This is just the player.
-}

module Mastermind
( arrangements
, getPoints
, makeGuess
, updatePool
) where

import Data.List (nub)

-- | Obtains all possible arrangements of [1,2,3,4,5,6] with a length of 4.
arrangements :: [[Int]]
arrangements = filter ((==4) . length) $
               map (nub) [x | x <- mapM (const [1,2,3,4,5,6]) [1..4]]

-- | Returns correct and regular points of the guess, given the answer.
getPoints :: Eq a => [a] -> [a] -> [Int]
getPoints answer guess =
  [c, r]
  where c = length $ filter id $ zipWith (==) answer guess
        r = (length . filter (`elem` answer) $ guess) - c

{-|
  Updates the pool given an existent pool, the last guess and its points,
  and returns the updated pool.
-}
updatePool :: [[Int]] -> [Int] -> [Int] -> [[Int]]
updatePool [] _ _ = []
updatePool (poolH:poolT) guess points =
    if (poolH /= guess && getPoints guess poolH == points) then
      poolH : updatePool poolT guess points
    else updatePool poolT guess points

-- | Given the current pool, the last guess and its points, makes a new guess.
makeGuess :: [[Int]] -> [Int] -> [Int]
makeGuess pool points =
  fst $ makeGuess' pool pool points

{-|
  Return a tuple containing the new best guess and its score.
  This is done by implementing the Knuth's algorithm.

  Algorithm description:
  For the head X of the current pool, we calculate how many of the elements in
  the whole pool match its points, if X was the answer. This number of matched
  elements is X's score.
  The best guess is the element with the minimal score.
-}
makeGuess' :: [[Int]] -> [[Int]] -> [Int] -> ([Int], Int)
makeGuess' wholePool [guess] points =
  (guess, score)
  where score = length $ updatePool wholePool guess points
makeGuess' wholePool (poolH : poolT) points
  | currScore <= score = (poolH, currScore)
  | otherwise          = (guess, score)
  where currScore = length $ updatePool wholePool poolH points
        (guess, score) = makeGuess' wholePool poolT points
