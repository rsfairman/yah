module Main where

{-

Prints the score of an optimally played game similar to Yahtzee: the
'WhyNot' version.

See YahRules.hs for the rules of the game and Yahtzee.hs for a discussion
of how this solution is found, along with the implementation.

This version makes various tweaks to speed things up, mostly to use Vectors
instead of lists. No doubt, there's a lot more that could be done, but I've
reached the end of my patience.

The program now takes five or six seconds to run single-threaded.

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
  
