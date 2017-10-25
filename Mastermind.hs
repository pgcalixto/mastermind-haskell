
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
) where

import Data.List (nub)

-- | Obtains all possible arrangements of [1,2,3,4,5,6] with a length of 4
arrangements :: [[Int]]
arrangements = filter ((==4) . length) $
               map (nub) [x | x <- mapM (const [1,2,3,4,5,6]) [1..4]]
