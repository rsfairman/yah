module Yahtzee where

{-

There are no major changes in this version ('RowVectorIO') compared to
the previous version ('RowVectorST'). This version does the calculation
in an IO monad, and it can then be threaded. The changes are minor, 
mostly to do with where and when the calculation "culminates" -- whether
runST is used or unsafePerformIO. There's some discussion and simple 
examples of using monads with mutable Vectors in 'RowVectorST'.

-}


import Data.List(sortOn)
import Data.Foldable(maximumBy)
import Data.Ord(comparing)
import Data.Bits(complementBit)
import qualified Data.Vector as Vb
import qualified Data.Vector.Mutable as Vm
import qualified Data.Vector.Generic.Mutable as Vg
import qualified Data.Vector.Generic as Gg
import Data.Traversable(forM)
import qualified Data.Foldable as Dfold(forM_)
import Control.Concurrent.Async (forConcurrently_)

import YahDice(RollIndex,RerollChoice,
            setRolls,allRROutcomes,rangeRollAndChoiceIndex,
            theRollProbs,getRRChoices,toRollChoiceIndex,indexAndRollToChoice)
import YahRules (getScore)



--- Inefficient, but easy to understand.
multiLookup :: [Int] -> [a] -> [a]
multiLookup indices list = map (list !!) indices

-------------------------------------------------------

-- Some simple math...

-- \sum a_i b_i
weightedSum :: [(Double,Double)] -> Double
weightedSum prs = sum $ map (\(a,b) -> a*b) $ prs

-- The first argument is the probability of each roll, the second is an EV.
-- score. Produces the weighted average score over these rolls.
-- Be careful: this assumes that the two lists have been sorted to a
-- consistent order.
weightedEVOfRolls :: [Double] -> [Double] -> Double
weightedEVOfRolls probs scores = weightedSum $ zip probs scores

-------------------------------------------------------

-- A bit of combinatorics....
-- This is much different than in earlier versions. What these functions do
-- is conceptually the same, but the details are different.
-- All the choices used by the program take the form n choose 13,
-- and these can be represented as an integer in [0..2^13-1]
-- rather than a [Bool].

-- The n-th entry of this list corresponds to the list of ways
-- one may choose among 13 items. So choiceList !! n is a list
-- 13 choose n long, where each element of that list, when interpreted
-- as a binary number, is a set of booleans indicating whether the 
-- item is chosen.

type RowChoiceAsBinary = Int;
choiceList :: [[RowChoiceAsBinary]]
choiceList = [ getChoiceList 13 k | k <- [0..13] ]

-- A single list for n choose k. So getChoiceList n k will be a list
-- of all ways (expressed in binary form) to do n choose k.
-- Thus,
-- getChoiceList 4 2 = [3,5,6,9,10,12]
-- which is equivalent to
-- [ [TTFF], [TFTF], [FTTF], [TFFT], [FTFT] , [FFTT] ]
-- The result is naturally sorted in ascending order.
getChoiceList :: Int -> Int -> [Int]
getChoiceList _ 0 = [0]
getChoiceList n 1 = [ 2^k | k <- [0..(n-1) ] ]
getChoiceList n k 
  | n == k = [2^n - 1]
  | otherwise = (getChoiceList (n-1) k) ++ (map ((2^(n-1)) +) $ getChoiceList (n-1) (k-1))

-- Given a particular choice (out of 13), return a list of
-- corresponding values in the range [1..13]. For a given
-- ChoiceAsBinary for a selection of rows, this returns
-- the row numbers selected.
-- Note that the result is in reverse order: largest values first.
getChoices :: RowChoiceAsBinary -> [Int]
getChoices c = go c 12
  where
    go 0 0 = []
    go _ 0 = [1]
    go d k
      | d >= 2^k  =  (k+1) : go (d-2^k) (k-1)
      | otherwise = go d (k-1)
  
-- Swap one of the binary entries in a choice, where
-- the entry (a row) is counted from 1, not zero.
swapBoolAt :: Int -> RowChoiceAsBinary -> RowChoiceAsBinary
swapBoolAt row c = c `complementBit` (row-1)
  


--------------------------------------------------------------

-- Now the meat...


type RowsOpen = RowChoiceAsBinary -- binary bit == 1 ==> row open.
type EV = Double
type RowChoice = Int -- in [1..13]

-- Under different scenarios (implicit from the context where these types
-- are used), these are the various choices for what the player might do,
-- and the EVs for those choices.
--
-- RowOptions indicates, for each possible RollSorted, the best row to
-- choose and the EV of that choice. RerollOptions is similar, but it 
-- indicates the best choice for which dice to reroll.
-- 
-- In this version, 'RollVectorST', the value of RowsOpen is no longer
-- explicitly stored; it's implict from the index at which an entry
-- appears in the monster table.

type RowOptions = [(RowChoice,EV)]
type RerollOptions = [(RerollChoice,EV)]

type MonsterEntry = (
        EV,            -- finalEV 
        RowOptions,    -- rrDoneStage
        RerollOptions, -- oneRRLeftStage
        RerollOptions  -- twoRRLeftStage
        )
  
monster :: IO (Vb.Vector MonsterEntry)
monster = do
  
  -- One entry for each of the 2^13 ways that rows may be chosen.
  m <- Vg.new 8192 :: IO (Vm.IOVector MonsterEntry)
  
  -- The case of no rows open is the base case.
  Vg.unsafeWrite m 0 (0,[],[],[])
  
  -- Loop over the number of open rows
  Dfold.forM_ [1..13] $ \i -> do
    
    -- The set of cases to be considered: a list of all ways in which i
    -- items (the open rows) may be chosen among 13.
    let allRowSets = choiceList !! i
    
    -- Do it in parallel:
    -- forConcurrently_ allRowSets $ \rowSet -> do
    -- Or single-threaded:
    Dfold.forM_ allRowSets $ \rowSet -> do
      
      -- NOTE: I had thought that using -XBangPatterns in the cabal file
      -- under ghc-options, and adding a bang (!) to entry would allow
      -- threading to be done, but it doesn't. It does work to use -XStrict.
      -- Probably, it's a matter of knowing exactly where bangs are needed.
      -- Here, it's easier to just use -XStrict, which has the effect of
      -- making everything strict by default. See
      -- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/strict.html
      rrDone <- doRRDone m rowSet
      let entry = pureCalc rrDone 
      Vg.unsafeWrite m rowSet $ (\(a,(b,c,d)) -> (b,a,c,d)) (rrDone,entry)
    
  Gg.unsafeFreeze m

  
-- In previous versions, this was done inside the table-making function,
-- but it seems better to pull it out and express it in pure form.
pureCalc:: RowOptions -> (EV,RerollOptions,RerollOptions)
pureCalc rrDone = (finalEV,oneRRLeft,twoRRLeft)
  where
      -- For a given rowSet, do the calculation for all roll/reroll choices
      -- outcomes only once. This saves time due to the many duplicates.
      --
      -- For every roll/reroll index value, this is the set of possible
      -- outcomes and their probability.
      rrOuts = allRROutcomes :: [[(RollIndex,Double)]]
      
      -- For each of these roll/reroll index outcomes, determine the EV.
      -- Remember, the set of rolls in each entry of rrOuts is limited to
      -- those outcomes that actually occur for the implied reroll choice,
      -- while the set of RollSorted values in rrDone includes all 252 
      -- possible forms of 5d6. Look up only the EVs in rrDone that are
      -- relevant for the weighted average and replace the roll with the
      -- EV of that roll.
      lookupEV aRoll = snd (rrDone !! aRoll)
      rr2EVs = ((map.map) (\(theRoll,theProb) -> (lookupEV theRoll,theProb)) rrOuts)
            :: [[(Double,Double)]]
      
      -- And convert these EVs and probabilities to a single weighted EV.
      -- Remember: these indices correspond to roll/reroll choices.
      choiceEVs = ( map weightedSum rr2EVs ) :: [Double]
      
      -- And the next set of choices and EVs for the player:
      oneRRLeft = getAllRRChoices choiceEVs
      
      -- Same thing again:
      lookup2 aRoll = snd (oneRRLeft !! aRoll)
      rr1EVs = ((map.map) (\(theRoll,theProb) -> (lookup2 theRoll,theProb)) rrOuts)
            :: [[(Double,Double)]]
      choices2 = ( map weightedSum rr1EVs ) :: [Double]
      twoRRLeft = getAllRRChoices choices2
      
      -- The final layer is simple:
      twoRRStripped = map snd twoRRLeft
      finalEV = weightedEVOfRolls theRollProbs twoRRStripped
  
      
-- This is uglier than in previous versions because the information we want
-- is inside a mutable vector.
doRRDone :: Vm.IOVector MonsterEntry -> RowsOpen -> IO RowOptions
doRRDone m rowsOpen = forM setRolls $ \roll -> do
    
    -- The list of open rows.
    let openRowIndices = (getChoices rowsOpen) :: [Int]
    
    -- For each of these open rows, see what the score would be if roll is
    -- used there, and the EV for all the *other* open rows.
    -- theChoices :: [(Int,Int,Double)]
    theChoices <- forM openRowIndices $ \whichRow -> do
      
      let unfilledRowsChoice = (swapBoolAt whichRow rowsOpen) :: Int
      otherEV <- Vg.unsafeRead m unfilledRowsChoice
      let priorEV = (\(ev,_,_,_) -> ev) otherEV
      
      return (whichRow,getScore whichRow roll,priorEV) :: IO (Int,Int,EV)
      
    -- Choose the row based on the combined EV:
    let combinedChoices = map (\(row,sa,e) -> (row,fromIntegral(sa)+e)) theChoices
    let (row,score) = maximumBy ( comparing snd ) combinedChoices
    
    return (row,score) -- :: ST s (RowOptions)



-- Changed in 'BetterAlg'. The argument is now the set of EVs corresponding to
-- the set of all roll/reroll index values.
getAllRRChoices :: [Double] -> RerollOptions
getAllRRChoices choiceEVs = do
    r <- setRolls
    
    -- For each roll, there are various reroll choices. The choiceEVs
    -- is for all possible roll/reroll combinations. Get only those reroll
    -- choices that are relevant to this roll.
    let rrChoices = getRRChoices r
              
    -- The set of indices in choiceEVs that are relevant.
    let index = (map (toRollChoiceIndex r) rrChoices) :: [Int]
              
    -- The particular entires of choiceEVs, tupled with the index.
    let relevantChoices = multiLookup index $ zip choiceEVs rangeRollAndChoiceIndex
              
    -- This might (?) be slower than what was done in the 'Initial'
    -- version since it uses 'last', but it's simpler.
    let (ev,bestIdx) = last $ sortOn fst relevantChoices
    
    -- See YahDice.hs. Noting the choice here is irrelevant to the EV
    -- calculation, but it's needed if we're to play the game optimally.
    -- Instead of the actual choice, it would also work to note bestIdx
    -- itself, which would be trivial here and save time, but would defer
    -- decoding it to the playing stage.
    let choice = indexAndRollToChoice bestIdx r
              
    return (choice,ev) -- (RerollChoice,EV)
  
  
  
