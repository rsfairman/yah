module YahDice (
  -- types
  RollSorted,
  RerollChoice,
  RollWithProb,
  -- values
  setRolls,
  -- functions
  getRRProbTable,
  theRollProbs
  )
where

    
{-

Basic information about dice and probabilities.

Because this information is used so extensively, the data is held as fixed
tables, and the external functions merely do a lookup.

A little math background for the curious...

There are (n+r-1) choose r ways to roll r n-sided dice. Why? It's well-known
that n choose k = n!/[k!(n-k)!] is the number of ways to choose k things 
among n. Imagine the numbers in the range 1 throught n+r-1, and r values
from this range are chosen (with no repeats), in the normal way that one
thinks of (n+r-1) choose r. Think of each value chosen as the value shown on
the face of a "dye" (note the misspelling; not a "die"). Shift this face
value down by (r-1) on the first dye, (r-2) on the second dye, etc., to obtain
die (not dye) values and you end up with exactly the set of possible outcomes
of rolling r n-sided dice. 

The number of outcomes in each case is:
5d6  252 outcomes
4d6  126 outcomes
3d6  56 outcomes
2d6  21 outcomes
1d6  6 outcomes

Most of the code here is related to the probabilities for various ways to
reroll a subset of five dice. There are 252 ways that 5d6 could be rolled.
For five dice, there are up to 2^5=32 ways that one might choose to reroll,
but many of these choices are identical. For example, there are only six
distinct ways that one could choose to reroll [1,1,1,1,1]: reroll none of
them, reroll one, etc. So, for each of the 252 intial rolls, there is some
number of distinct ways to reroll. The total of these distinct ways over all
252 starting points is 4,368.

For each reroll choice, there will be some number of outcomes for the dice
rerolled (as noted above). The total of these outcomes, summed over the 4,368
reroll choices, is 254,100.

The precise values, 4,368 and so forth, and not important for what follows,
but help to undestand the size of the problem.

-}

import Data.Maybe (fromJust)
import Data.Foldable(find)
import Data.List(sort,sortBy)
import Data.Function(on)

----------------------------------------------------------

-- Some very elementary probability calculations...

-- A set of dice rolls may be unordered or sorted in ascending order. Sorting
-- puts a roll into a canonical form since.
type Roll = [Int]
type RollSorted = [Int]

-- Provides the total set of ways that n colored dice could come out.
-- The number of ways ndk (colored!) could come out is k^n.
allNd6 :: Int -> [Roll]
allNd6 0 = [[]]
allNd6 n = [ m : suf | m <- [1..6], suf <- allNd6 (n-1) ]

-- The "set" of nd6, meaning that all duplicates have been eliminated after
-- sorting. So, (setNd6 5) is the 252 values for 5d6.
-- Thanks to Dan Pratt for this.
setNd6 :: Int -> [RollSorted]
setNd6 n = gen n (1,6)
  where
    -- A slightly more general case:
    gen 0 _ = [[]]
    gen k (lo,hi) = [ m : suf | m <- [lo..hi], suf <- gen (k-1) (m,hi) ]

-- A table providing the probability of each possible roll. No doubt there's
-- a way to do this algebraically, but let the computer do it...as long
-- as it's done only once and not continuously recalculated.
probsNd6 :: Int -> [(RollSorted,Double)]
probsNd6 n = [ (r,fromIntegral(doCount r) / fromIntegral(length allSorted)) | r <- setNd6 n]
  where
    allSorted = map sort (allNd6 n)
    doCount r = length [m | m <- allSorted, m == r ]

-- Create a lookup table so that we're not recalculating.
theRollProbs :: [(RollSorted, Double)]
theRollProbs = probsNd6 5

-- For external use: all ways to roll 5d6.
setRolls :: [RollSorted]
setRolls = setNd6 5

---------------------------------------------------

-- Now some trickier stuff related to rerolling a subset of five dice.

-- When rerolling five dice, there are 2^5 possible choices for how one 
-- might reroll. Here they are:
type RerollChoice = [Bool]

allRR :: [RerollChoice]
allRR = [[b1,b2,b3,b4,b5] | b1<-[True,False],b2<-[True,False],b3<-[True,False],b4<-[True,False],b5<-[True,False]]

-- For a given 5-tuple of booleans and a given roll, return
-- the list of die face values being rerolled.
whichFaces :: Roll -> RerollChoice -> Roll
whichFaces xs bs = map fst $ filter (\(_,b) -> b) $ zip xs bs

-- This takes a roll, and a list of reroll choices yet to test, and a list of
-- choices and reroll outcomes that have been "accepted," so far, as distinct.
-- It works through the given choices to test and determines whether these
-- choices lead to new outcomes, and returns the list of these accepted 
-- choices. It's like an oddball filter.
recurDiffRRs :: Roll -> [RerollChoice] -> ([Roll],[RerollChoice]) -> ([Roll],[RerollChoice])
recurDiffRRs roll (c:[]) (as,bs) = if knownRoll then (as,bs) else (wfRoll:as,c:bs) 
  where 
    wfRoll = whichFaces roll c
    knownRoll = wfRoll `elem` as
recurDiffRRs roll (c:cs) (as,bs) = if knownRoll then recurDiffRRs roll cs (as,bs) else recurDiffRRs roll cs (wfRoll:as,c:bs) 
  where
    wfRoll = whichFaces roll c
    knownRoll = wfRoll `elem` as
recurDiffRRs _ _ _ = error "fell through recurDiffRRs"

-- The thing we care about: for each RollSorted, it provides the list of
-- distinct reroll choices.
rerollChoices :: [(RollSorted,[RerollChoice])]
rerollChoices = [ (r,choices r) | r<-setRolls ]
  where
    choices r = snd $ recurDiffRRs r allRR ([],[])

-- Convenient lookup in the above.
getRRChoices :: RollSorted -> [RerollChoice]
getRRChoices r = snd $ fromJust $ find (\t -> fst t == r ) rerollChoices

----------------------------------------------------

-- The above determines, for each initial roll, the set of distinct ways
-- one might choose to reroll. Now, for each initial roll, and each reroll
-- choice, what are the possible outcomes and what are their probabilities?
--
-- Note: don't get confused if you examine the output. For example,
-- rrProbTableII [1,1,1,1,1] [True,False,False,False,False]
-- is *supposed* to output
-- [([1,1,1,1,1],0.16),([1,1,1,1,2],0.16),([1,1,1,1,3],0.16),([1,1,1,1,4],0.16),([1,1,1,1,5],0.16),([1,1,1,1,6],0.16)]
-- What's confusing is that the call asks for the first die to be 
-- rerolled, so you expect to see something more like [1,1,1,1,1], [2,1,1,1,1], 
-- etc., but the roll values must be in sorted form (RollSorted).

type RollWithProb = (RollSorted,Double)
rrProbTableII :: RollSorted -> RerollChoice -> [RollWithProb]
rrProbTableII roll choice = sortBy (compare `on` fst) unsorted
  where
    unsorted = map (\(rr,p) -> (sort $ notRerolled ++ rr,p)) theRRs
      where
        -- Pull out the die faces which are not being rerolled
        notRerolled = map fst $ filter (\(_,b) -> not b) $ zip roll choice :: [Int]
        
        -- The number of dice being rerolled
        countRerolled = sum $ map fromEnum choice

        -- And the set of rerolls, with probabilities:
        theRRs = probsNd6 countRerolled :: [(RollSorted,Double)]

-- The big table that we really want: not with 'II'.
-- For every possible roll, there is a set of reroll choices, and 
-- for each of these choices, there is a set of outcomes, with probability.
type ChoicesAndOutcomes = (RerollChoice,[RollWithProb])
rrProbTable :: [(RollSorted,[ChoicesAndOutcomes])]
rrProbTable = [ (r,outcomes r) | r<-setRolls ]
  where
    outcomes r = [ (choice,rrProbTableII r choice)| choice <- getRRChoices r]

-- Convenience lookup for a particular roll to be rerolled.
getRRProbTable :: RollSorted -> [ChoicesAndOutcomes]
getRRProbTable r = snd $ fromJust $ find (\t -> (fst t == r)) rrProbTable



