module Main where

{-

Prints the score of an optimally played game similar to Yahtzee: the
'RowVectorST' version.

See YahRules.hs for the rules of the game and Yahtzee.hs for a discussion
of how this solution is found, along with the implementation.

This version uses a single Int value instead of a [Bool] (type RowsOpen in
Yahtzee.hs) and it does so using a mutable vector that's calculated within
an ST monad.

The program now takes about 70 seconds to run single-threaded.

-}

import qualified System.CPUTime
import qualified Data.Time.Clock
import Control.Parallel(pseq)
import qualified Data.Vector as Vb


import Yahtzee (monster)


getCPUTimeSecs :: IO Double
getCPUTimeSecs = do
  t <- System.CPUTime.getCPUTime
  
  -- Convert from pice seconds to seconds.
  return $ (fromInteger t) * 1e-12


main :: IO ()
main = do
  c1 <- getCPUTimeSecs
  t1 <- Data.Time.Clock.getCurrentTime
  
  -- This prints the after fully evaluating the entire table.
  -- A fair amount of the data in this table has to do with things like the
  -- correct reroll choice to make or which row to use with a particular final
  -- roll. Some of that data isn't strictly necessary to obtain the EV, but 
  -- the full table is necessary if you want to play the game optimally. It's 
  -- unclear (based on timing) whether Haskell is calculating this stuff or
  -- not without the pseq, but add it to be certain.   
  monster `pseq` print $ "EV of an optimally played game is " ++
      (show $ (\(ev,_,_,_) -> ev) (monster Vb.! 8191))
      
  cn <- getCPUTimeSecs
  tn <- Data.Time.Clock.getCurrentTime
  print $ "full run elapsed user secs: " ++ (show $ Data.Time.Clock.diffUTCTime tn t1)
  print $ "full run elapsed CPU secs: " ++ show (cn-c1)
  
