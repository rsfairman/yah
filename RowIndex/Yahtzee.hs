module Yahtzee where

{-

Major changes in this version: 'RowIndex'...

All previous versions use a [Bool] to specify the set of open rows (type
RowsOpen). Instead, use a single Int, where each binary bit is treated
like a boolean value. Otherwise the algorithm remains the same.

-}

import Data.Function(on)
import Data.List(sortBy,sortOn)
import Data.Foldable(find,maximumBy)
import Data.Maybe (fromJust)
import Data.Ord(comparing)
import Control.Parallel.Strategies (parList,rdeepseq,rpar,rparWith,rseq,runEval,using,parEval)
import Control.DeepSeq(force,deepseq)
import Control.Parallel(pseq)
import Data.Bits(complementBit)

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
-- In this version, 'RollIndex', the particular roll is no longer explicit.
-- The rolls are indexed by a value in the range [0..251] and a particular
-- roll is implied by the index at which a value appears.

type RowOptions = [(RowChoice,EV)]
type RerollOptions = [(RerollChoice,EV)]

monster :: [
        [(
        RowsOpen,      -- Must match the current index of the outer list
        EV,            -- finalEV 
        RowOptions,    -- rrDoneStage
        RerollOptions, -- oneRRLeftStage
        RerollOptions  -- twoRRLeftStage
        )]
        ]

monster = [ entry i | i <- [0..13] ]
  where
    entry :: Int -> [(RowsOpen,EV,RowOptions,RerollOptions,RerollOptions)]
    
    -- Base case of recursion: no rows open.
    entry 0 = [(0,0,[],[],[])]
    
    -- Other entries depend on the previous finalEV stage (and nothing else).
    -- In parallel:
    entry n = go (getFinalEV (entry (n-1)) ) `using` parList rdeepseq
    -- or single-threaded:
    -- entry n = go (getFinalEV (entry (n-1)) )
      where
        go prevFinal = do
          -- rowSet <- getChoiceList 13 n
          -- A list of binary values, representing packed booleans.
          rowSet <- choiceList !! n
          
          -- RowOptions = [(RowChoice,EV)]
          -- Note that this is now ('RollIndex') implicitly indexed.
          let rrDone = (getRRDone prevFinal rowSet) :: RowOptions
          
          -- For a given rowSet, do the calculation for all roll/reroll choices
          -- outcomes only once. This saves time due to the many duplicates.
          --
          -- For every roll/reroll index value, this is the set of possible
          -- outcomes and their probability.
          let rrOuts = allRROutcomes :: [[(RollIndex,Double)]]
          
          -- For each of these roll/reroll index outcomes, determine the EV.
          -- Remember, the set of rolls in each entry of rrOuts is limited to
          -- those outcomes that actually occur for the implied reroll choice,
          -- while the set of RollSorted values in rrDone includes all 252 
          -- possible forms of 5d6. Look up only the EVs in rrDone that are
          -- relevant for the weighted average and replace the roll with the
          -- EV of that roll.
          let lookupEV aRoll = snd (rrDone !! aRoll)
          let rr2EVs = ((map.map) (\(theRoll,theProb) -> (lookupEV theRoll,theProb)) rrOuts)
                :: [[(Double,Double)]]
          
          -- And convert these EVs and probabilities to a single weighted EV.
          -- Remember: these indices correspond to roll/reroll choices.
          let choiceEVs = ( map weightedSum rr2EVs ) :: [Double]
          
          -- And the next set of choices and EVs for the player:
          let oneRRLeft = getAllRRChoices choiceEVs
          
          -- Same thing again:
          let lookup2 aRoll = snd (oneRRLeft !! aRoll)
          let rr1EVs = ((map.map) (\(theRoll,theProb) -> (lookup2 theRoll,theProb)) rrOuts)
                :: [[(Double,Double)]]
          let choices2 = ( map weightedSum rr1EVs ) :: [Double]
          let twoRRLeft = getAllRRChoices choices2
          
          -- The final layer is simple:
          let twoRRStripped = map snd twoRRLeft
          let finalEV = weightedEVOfRolls theRollProbs twoRRStripped
          
          return (rowSet,finalEV,rrDone,oneRRLeft,twoRRLeft)
  


-- The functions that follow could have been defined within the monster
-- calculation, but pulling them out makes things tidier and easier to
-- understand.


-- Given one entry of monster, pull out the finalEV part of the data.
getFinalEV :: [(RowsOpen,EV,RowOptions,RerollOptions,RerollOptions)] -> [(RowsOpen,EV)]
getFinalEV entireRow = map (\(opens,ev,_,_,_) -> (opens,ev)) entireRow

-- Convenience lookup. SLOW!
findFinal :: [(RowsOpen,EV)] -> RowsOpen -> EV
findFinal list which = snd $ fromJust $ find (\(possible,_) -> possible == which) list


-- Takes the EVs from the finalEV stage, together with a particular choice
-- of open rows, and returns the best row to choose for each roll and the EV.
getRRDone :: [(RowsOpen,EV)] -> RowsOpen -> RowOptions
getRRDone prevFinal rowsOpen = do
    roll <- setRolls
    
    -- For each open row in rowsOpen, determine the sum of the score if roll 
    -- is used to fill that row *plus* the EV for the remaining unfilled
    -- rows. Choose the row that provides the greatest sum.
    
    -- The list of open rows. Add 1 since we want the row numbers,
    -- not the indices in the list.
    -- let openRowIndices = map (+1) $ boolToIndex True rowsOpen
    let openRowIndices = getChoices rowsOpen
    
    -- For each of these open rows, see what the score would
    -- be for roll, and the EV for all the *other* open rows.
    let theChoices = -- trace (show openRowIndices ++ " " ++ show rowsOpen) $ [
          [
          (whichRow,
            getScore whichRow roll,
            findFinal prevFinal (swapBoolAt whichRow rowsOpen)
          ) | whichRow <- openRowIndices] 
    
    -- Choose the row based on the combined EV:
    let combinedChoices = map (\(row,sa,e) -> (row,fromIntegral(sa)+e)) theChoices
    let (row,score) = maximumBy ( comparing snd ) combinedChoices
    
    return (row,score)


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

