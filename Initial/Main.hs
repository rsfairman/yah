module Main where

{-

This may not be the best version to begin with. This is the 'Initial'
version and it works, but may not be as easy to understand as the 'Cleaner'
version. Given how the algorithm is expressed, it's also a little mysterious
how the compiler is able to sequence the various layers of computation in
the necessary order.


Prints the score of an optimally played game similar to Yahtzee.

See YahRules.hs for the rules of the game and Yahtzee.hs for a discussion
of how this solution is found, along with the implementation.

This version takes some time to run, so be prepared to wait. Single-threaded,
it might take more than an hour, depending on your machine.

-}

import qualified System.CPUTime
import qualified Data.Time.Clock

import Yahtzee (finalEV)


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
      (show $ snd $ last $ snd $ last finalEV)
  
  cn <- getCPUTimeSecs
  tn <- Data.Time.Clock.getCurrentTime
  print $ "full run elapsed user secs: " ++ (show $ Data.Time.Clock.diffUTCTime tn t1)
  print $ "full run elapsed CPU secs: " ++ show (cn-c1)
  
