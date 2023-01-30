module Main where

{-
Prints the score of an optimally played game similar to Yahtzee: 
the 'Cleaner' version.

See YahRules.hs for the rules of the game and Yahtzee.hs for a discussion
of how this solution is found, along with the implementation.

Does the calculation as a single "monster" table rather than four related
tables. Otherwise, it's the same as what's in Initial, and is probably 
easier to understand. This version fails to parallize well. See Yahtzee.hs.

This version takes some time to run, so be prepared to wait. Single-threaded,
it might take more than an hour, depending on your machine.

-}

import qualified System.CPUTime
import qualified Data.Time.Clock

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
  
  print $ "EV of an optimally played game is " ++
      (show $ map (\(_,ev,_,_,_) -> ev) $ last monster)
      
  cn <- getCPUTimeSecs
  tn <- Data.Time.Clock.getCurrentTime
  print $ "full run elapsed user secs: " ++ (show $ Data.Time.Clock.diffUTCTime tn t1)
  print $ "full run elapsed CPU secs: " ++ show (cn-c1)
  
