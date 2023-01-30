module Main where

{-

Prints the score of an optimally played game similar to Yahtzee: the
'RowVectorIO' version.

See YahRules.hs for the rules of the game and Yahtzee.hs for a discussion
of how this solution is found, along with the implementation.

This version uses a single Int value instead of a [Bool] (type RowsOpen in
Yahtzee.hs) and it does so using a mutable vector that's calculated within
the IO monad. Using IO (instead of ST) allows the calculation to be
threaded. This required some small changes to this file, and to Yahtzee.hs.

The program now takes about 60 seconds to run single-threaded.

-}

import qualified System.CPUTime
import qualified Data.Time.Clock
import qualified Data.Vector as Vb
import System.IO.Unsafe(unsafePerformIO)


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
  
  let m = unsafePerformIO (monster)
  
  print $ "EV of an optimally played game is " ++
      (show $ (\(ev,_,_,_) -> ev) (m Vb.! 8191))
      
  cn <- getCPUTimeSecs
  tn <- Data.Time.Clock.getCurrentTime
  print $ "full run elapsed user secs: " ++ (show $ Data.Time.Clock.diffUTCTime tn t1)
  print $ "full run elapsed CPU secs: " ++ show (cn-c1)
  
