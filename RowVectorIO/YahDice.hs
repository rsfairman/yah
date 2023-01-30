module YahDice (
  -- types
  RollSorted, -- To be avoided outside.
  RollIndex,
  RerollChoice,
  -- values
  setRolls,
  allRROutcomes,
  rangeRollAndChoiceIndex,
  -- functions
  toRollSorted,
  getRRChoices,
  toRollChoiceIndex,
  indexAndRollToChoice,
  theRollProbs
  )
where


{-

Basic information about dice and probabilities.

There's a long discussion in previous versions about the changes relevant
to those versions. In this version (RollIndex), the important change is
some modest rearrangement so that the algorithm can work with indices
rather than lists of die values. For example, the roll [1,1,3,4,4]
might correspond to index 47. Integers are faster to work with than lists.

Lists *are* easier to work with here, even though they're slow. In theory,
certain uses of Roll and RollSorted could be eliminated here too to speed
things up. But the calculations done here should be a small part of the
overall picture.

-}

import Data.Maybe (fromJust)
import Data.Foldable(find)
import Data.List(elemIndex,sort,sortBy)
import Data.Function(on)

----------------------------------------------------------

-- Some very elementary probability calculations...

-- A set of dice rolls may be unordered or sorted in ascending order. Sorting
-- puts a roll into a canonical form since.
type Roll = [Int]
type RollSorted = [Int]
type RollIndex = Int

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
theRollProbs :: [Double]
theRollProbs = map snd $ probsNd6 5

-- Used below. Convert nd6 to an index relative to that set.
-- So toRollSubIndex 3 [2,3,4] would return 27 since that roll is
-- the 27th (counting from zero) among all sorted 3d6.
toRollSubIndex :: Int-> RollSorted -> Int
toRollSubIndex n r = fromJust $ elemIndex r (setNd6 n)

-- For external use: all ways to roll 5d6.
setRolls :: [RollIndex]
setRolls = [0..251]

-- The code below needs this when building a probability table for rerolls.
setRollsSorted :: [RollSorted]
setRollsSorted = setNd6 5

-- Below, we need to map from RollSorted to RollIndex.
-- Note that this maps to the index, NOT index + 1, to start at 0.
toRollIndex :: RollSorted -> RollIndex
toRollIndex r = fromJust $ elemIndex r setRollsSorted

-- Going the other way is easy.
toRollSorted :: RollIndex -> RollSorted
toRollSorted i = setRollsSorted !! i


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

-- Same thing, but returns the list of faces *not* being rerolled.
notFaces :: Roll -> RerollChoice -> Roll
notFaces xs bs = map fst $ filter (\(_,b) -> not b) $ zip xs bs

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
rerollChoices = [ (r,choices r) | r<-setRollsSorted ]
  where
    choices r = snd $ recurDiffRRs r allRR ([],[])

getRRChoices :: RollIndex -> [RerollChoice]
getRRChoices r = snd $ fromJust $ find (\t -> fst t == toRollSorted r ) rerollChoices

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


-------------------------------------------

-- What follow is the new method, based on two tables...
--
-- The idea is that each initial roll and reroll choice corresponds to an
-- index value. Here is that correspondence going in one direction.
-- It's important to remember that this is a many-to-one function; indeed,
-- that is the whole point.

type RollAndChoiceIndex = Int

toRollChoiceIndex :: RollIndex -> RerollChoice -> RollAndChoiceIndex
toRollChoiceIndex roll cs = theIndex (notFaces (toRollSorted roll) cs) (5 - (sum $ map fromEnum cs))
  where
    -- fs = faces not being rerolled
    -- n = number of dice not being rerolled.
    theIndex _  0 = 0
    theIndex fs 1 = 1 + toRollSubIndex 1 fs  -- [1..6]
    theIndex fs 2 = 1 + 6 + toRollSubIndex 2 fs  -- [7..27]
    theIndex fs 3 = 1 + 6 + 21 + toRollSubIndex 3 fs  -- [28..83]
    theIndex fs 4 = 1 + 6 + 21 + 56 + toRollSubIndex 4 fs -- [84..209]
    theIndex fs 5 = 1 + 6 + 21 + 56 + 126 + toRollSubIndex 5 fs -- [210..461]
    theIndex _  _ = error "problem in YahDice.theRollChoiceIndex"


-- And the inverse function. This inverts a many-to-one function, so it
-- makes an arbitrary choice about which inverse value to return.
--
-- Fortunately this isn't used much, but wow, this is inefficient
-- and generally horrible!

indexToRollChoice :: RollAndChoiceIndex -> (RollSorted,RerollChoice)
indexToRollChoice i = (roll,choice)
  where
    -- The number of dice not rerolled.
    bin
      | i > 461  = error "index too large in indexToRollChoice"
      | i >= 1+6+21+56+126 = 5 -- 210
      | i >= 1+6+21+56     = 4
      | i >= 1+6+21        = 3
      | i >= 1+6           = 2
      | i >= 1             = 1
      | i >= 0             = 0
      | otherwise = error "index negative in indexToRollChoice"
    
    -- The index of the sub-roll consisting of those dice that are not rerolled.
    subIndex
      | bin == 0 = 0
      | bin == 1 = i - 1
      | bin == 2 = i - 1 - 6
      | bin == 3 = i - 1 - 6 - 21
      | bin == 4 = i - 1 - 6 - 21 - 56
      | bin == 5 = i - 1 - 6 - 21 - 56 - 126
      | otherwise = error "bad bin value in indexToRerollChoice"
      
    -- The actual sub-roll.
    subRoll = (setNd6 bin) !! subIndex
    
    -- Expand the subRoll by repeating the last face to reach the full length.
    roll = tackOn subRoll
      where
        tackOn r
            -- If length is zero, then the subRoll was empty and all
            -- the dice are being rerolled. The value doesn't matter, but
            -- it has to be something.
          | null r        = tackOn [1]
          | length r == 5 = r
          | length r < 5  = tackOn (r ++ [last r])
          | otherwise = error "roll bug in indexToRollChoice"
          
    choice
      | bin == 0 = [True,True,True,True,True]
      | bin == 1 = [False,True,True,True,True]
      | bin == 2 = [False,False,True,True,True]
      | bin == 3 = [False,False,False,True,True]
      | bin == 4 = [False,False,False,False,True]
      | bin == 5 = [False,False,False,False,False]
      | otherwise = error "choice bug in indexToRollChoice"

-- The indexToRollChoice function above is correct, but it's not sufficient
-- as a record of what to do during a game. When analyzing the game, we
-- start with a roll and determine the best roll/choice index, when we will
-- want to know (during play) which choice to make relative to the roll
-- we have. Given the roll and the roll/choice index, one could figure out
-- the correct choice at that point in the game, but that's contrary to the
-- goal of making things on the playing side as trivial as possible. We want
-- to record a reroll choice in the optimization process that's directly
-- suitable to the roll at that point in the game. 
--
-- NOTE: The implementation below replicates much of what is in 
-- indexToRollChoice, and the function above is probably unneccesary.
--
-- The idea of the implementation is to figure out, from the given
-- RollAndChoiceIndex, the value of the sub-roll for those faces that are
-- *not* to be rerolled. Compare that to the given RollSorted, and work
-- out which of the RollSorted values might change due to the reroll.
indexAndRollToChoice :: RollAndChoiceIndex -> RollIndex -> RerollChoice
indexAndRollToChoice i roll = theChoice
  where
    -- The number of dice not rerolled.
    bin
      | i > 461  = error "index too large in indexAndRollToChoice"
      | i >= 1+6+21+56+126 = 5 -- 210
      | i >= 1+6+21+56     = 4
      | i >= 1+6+21        = 3
      | i >= 1+6           = 2
      | i >= 1             = 1
      | i >= 0             = 0
      | otherwise = error "index negative in indexAndRollToChoice"
    
    -- The index of the sub-roll consisting of those dice that are not rerolled.
    subIndex
      | bin == 0 = 0
      | bin == 1 = i - 1
      | bin == 2 = i - 1 - 6
      | bin == 3 = i - 1 - 6 - 21
      | bin == 4 = i - 1 - 6 - 21 - 56
      | bin == 5 = i - 1 - 6 - 21 - 56 - 126
      | otherwise = error "bad bin value in indexAndRollToChoice"
      
    -- The actual sub-roll. These are the values of the faces of the dice
    -- that are not rerolled.
    subRoll = (setNd6 bin) !! subIndex
    
    -- Step through r (the given roll) and whereever it does not match
    -- subRoll is a die that is rerolled. Put another way, wherever
    -- r and subRoll *do* match is where True should appear in the answer.
    rollSorted = toRollSorted roll
    theChoice = reverse $ go [] rollSorted subRoll
      where
        go cs [] _ = cs
        go cs (r:rs) sub@(_:ss) 
          | r `elem` sub = go (False:cs) rs ss
          | otherwise = go (True:cs) rs sub
        go cs (_:rs) [] = go (True:cs) rs []
        go _ _ _ = error ("Impossible error in indexAndRollToChoice" ++
            " for " ++ show i ++ " " ++ show roll ++ " " ++show subRoll)

-- For every RollAndChoiceIndex, a probability table of the outcomes.
-- This is the entire point of this version. The map
-- (roll,reroll choice) -> RollAndChoiceIndex
-- is many-to-one, and there is no reason (other than simplicity!)
-- to use the direct map
-- (roll,reroll choice) -> [(roll outcomes,probability)]
-- since that is many-to-one.
rangeRollAndChoiceIndex :: [Int]
rangeRollAndChoiceIndex = [0..461]


-- Think of this as a map from RollAndChoiceIndex to the outcome probability
-- table, where the domain is the set of indices.
-- For each initial roll and reroll choice (which happens to correspond to a
-- RollAndChoiceIndex), there is a set of possible outcomes given by a
-- list of RollIndex values, and these outcomes have some probability.
allRROutcomes :: [[(RollIndex,Double)]]
allRROutcomes = byIndex
  where
    listActual = (map indexToRollChoice rangeRollAndChoiceIndex)
      :: [(RollSorted,RerollChoice)]
    bySorted = (map (\(rs,c) -> rrProbTableII rs c) listActual)
      :: [[(RollSorted,Double)]]
    byIndex = ((map.map) (\(rs,p) -> (toRollIndex rs,p)) bySorted)
      :: [[(RollIndex,Double)]]





