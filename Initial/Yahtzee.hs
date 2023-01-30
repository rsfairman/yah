module Yahtzee where

{-

NOTE: It might be better to look at the 'Cleaner' version before this one.
It's also unclear how the compiler is able to determine the correct
order of evaluation in this version. Each layer of the calculation refers 
to the previous layer by lookup, using a 'find' or '!!'. The compiler is
unable to "see through" these uses of 'find' and '!!" (or so I assume) to
know which aspects of a "previous layer" are necessary to proceed with a
current calculation, yet this does work. It must (?) be that the compiler
observes a dependence of A on B and, since it can't work out the details,
the compiler throws up its hands and just calculates (reduces to normal form)
every thing in B before attempting to calculate A. Either that or we got lucky.


Determine the optimal strategy for the game whose rules are found in 
YahRules.hs, where "optimal" means highest expected average score (the 
"expected value" or EV). A more sophisticated player might take other factors 
into account. When there are opposing players, the choice leading to the 
highest probabily of winning will be influenced by the score of the other 
player. Or we might play in such a way that we are willing to accept more 
frequent losses if we absolutely crush the other players once in a while -- 
because it's more psychologically satisfying, perhaps.

Consider how the problem might be approached very naively. You might start
at the begining of the game and build a tree out to the end of the game.
That is, to make the very first decision (what to reroll after the initial
roll), consider each possible reroll, its set of outcomes and their
probabilities; then, for each of these outcomes, do the same thing, out to 
the final move of the game. In principle, this would give you an EV for 
each of the initial reroll choices.

There are 252 ways that 5d6 can come up, and there are (roughly, on average)
something like 20 ways (out of 32) that one may choose to reroll, and each
of these reroll choices has numerous outcomes. In fact, there are 254,100
different "paths" from the 252 initial rolls to all of the possible outcomes. 
Estimate that as 2^18. So, the forward-looking tree that one might build would
have roughly (2^18)^2 = 2^36 nodes. In fact, one could find a lot of 
redundance, so the true size might be more like 2^30. After working through 
these 2^30 (ish) nodes, choose which row to fill in among the remaining open
rows. This will vary from 13 down to 1, but estimate this as 2^3 on average.
Finally, all of this must be done 13 times, so the total number of nodes to
examine is roughly
(2^30 x 2^3)^13 ~ 2^400.
So that's not going to happen! 

In contrast, the dynamic programming approach starts at the end of the game
and works forward to the begining of the game. More details are given below,
but in summary, there are 254,100 + 254,100 (plus, not times) decisions to
make regarding what to reroll; call this 2^19 (an over-estimate). There are
2^13 states for which rows remain open, so the total number of cases to 
examine is roughly
2^19 x 2^13 = 2^32.
That's a big number, but it's well within reach.

Here's how the dynamic program algorithm works...

Suppose that a certain set of the rows remain unfilled.

After all the rerolls, the final roll is known, and we must choose the best of
the remaining open rows to fill. To do this, for each row choice there is a
marginal increase in score (the value that goes into that row), plus the EV 
for filling the other open rows with future rolls. Choose the row that gives 
the highest combined value. Call this Stage 1 or rrDoneStage

If there's one reroll left, then figure out the EV of each choice for what
to reroll, just as above. That is, each reroll choice leads to various 
outcomes and probabilities; use Stage 1 to determine the EV for each of 
these choices, and pick the reroll choice that gives the greatest value. 
Call this Stage 2 or oneRRLeftStage. 

If there are two rerolls left, then it's just as above, except that the
decision is based on Stage 2. For each initial roll of a turn, this provides
the EV. Call this Stage 3 or twoRRLeftStage.

Note that the reroll choices in Stage 2 and Stage 3 aren't needed for the 
calculation of the EV of the game as a whole, but if the resulting data is to 
be used to play the game, then this information is obviously necessary.

Finally, as a basis for the Stage 1 calculation, for each set of filled rows, 
we want the EV for filling the remaining open rolls. To do this, for each set 
of open rows, look to Stage 3. Call this Stage 0/4, or finalEV.

-}

import Data.Function(on)
import Data.List(sortBy,sortOn)
import Data.Foldable(find,maximumBy)
import Data.Maybe (fromJust)
import Data.Ord(comparing)
import Control.Parallel.Strategies (parList,rdeepseq,using)

import YahDice(RollSorted,RerollChoice,RollWithProb,
            setRolls,theRollProbs,getRRProbTable)
import YahRules (getScore)


-------------------------------------------------------

-- Some simple math...

-- \sum a_i b_i
weightedSum :: [(Double,Double)] -> Double
weightedSum prs = sum $ map (\(a,b) -> a*b) $ prs

-- The first argument is the probability of each roll, the second is an EV.
-- score. Produces the weighted average score over these rolls.
weightedEVOfRolls :: [(RollSorted,Double)] -> [(RollSorted,Double)] -> Double
weightedEVOfRolls probs scores = weightedSum zipped
  where
    zipped = zip probPart scorePart
      where
        -- Sort these two, just to be sure (even though it's slow).
        probPart = map snd $ sortBy (compare `on` fst) probs
        scorePart = map snd $ sortBy (compare `on` fst) scores

-------------------------------------------------------

-- A bit of combinatorics....

-- getChoiceList n k returns a list of the n choose k possibilities,
-- where each item in the list is a lists of booleans indicating chosen or not.
-- For example,
-- getChoiceList 4 2 = [ [TTFF], [TFTF], [TFFT], [FTFT] , [FFTT], [FTTF] ]
-- for the six ways to choose 2 items among 4.
--
-- This is based on the recursive defintion of nCk:
-- nCk = (n-1)Ck + (n-1)C(k-1).

getChoiceList :: Int -> Int -> [[Bool]]
getChoiceList n 0 = [replicate n False]
getChoiceList n 1 = [ a(i) | i <- [0..(n-1)] ]
  where
    a i = replicate i False ++ [True] ++ replicate (n-i-1) False

getChoiceList n k 
  | n == k    =  [replicate n True] -- Only one way to do n choose n
  | otherwise = fpart ++ tpart
      where
        fpart = map (++[False]) $ getChoiceList (n-1) k
        tpart = map  (++[True]) $ getChoiceList (n-1) (k-1)

-- Converts [Bool] to a list of indices of the True values.
boolToIndex :: Bool -> [Bool] -> [Int]
boolToIndex which bs = map snd $ filter (\(b,_) -> which == b) $ zip bs [0..]

-- Replace the nth element of a list. 
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n replacment xs = z ++ (replacment:b)
  where 
    (z,(_:b)) = splitAt n xs

-- Take a [Bool] and "not" the nth entry.
swapBoolAt :: Int -> [Bool] -> [Bool]
swapBoolAt n bs = replaceAtIndex n (not $ bs!!n) bs


--------------------------------------------------------------

-- Now the meat...

-- This is the thing we ultimately want to calculate: finalEV, in particular
-- the case with zero rows open.

type RowsOpen = [Bool] -- True ==> row open.
type Score = Double -- Generally used as EV
type ScoreEV = (RowsOpen,Score)
type NumberRowsOpen = Int

finalEV :: [(NumberRowsOpen,[(RowsOpen,Score)])]
finalEV = [ (n, evs n) | n <- [1..13] ]
  where
      -- Let's try this in parallel!
      -- evs n = [ (rowSet,ev rowSet) | rowSet <- getChoiceList 13 n ] `using` parList rdeepseq
      -- Or single threaded:
      evs n = [ (rowSet,ev rowSet) | rowSet <- getChoiceList 13 n ]
        where
          ev rowSet = weightedEVOfRolls theRollProbs (get2RRLeft n rowSet)

-- Return the expected value for when the given set of rows is open.
-- Note that if there are NO rows open, then the EV of filling them is zero.
getFinalEV :: RowsOpen -> Score
getFinalEV fs
  | (sum $ map fromEnum fs) == 0 = 0
  | otherwise = snd $ fromJust $ find (\(bs,_) -> bs == fs) layerA 
  where
    openCount = (sum $ map fromEnum fs) :: Int
    layerA = (snd $ fromJust $ find (\(num,_) -> num == openCount) finalEV) 
          :: [(RowsOpen,Score)]


-- Stage 3 or twoRRLeftStage. For every possible set of open rows, 
-- determine the best reroll choice and the EV of that choice.

-- For a given roll, the best RR choice, and its EV.
type BestRerollChoice = (RollSorted,RerollChoice,Score)

twoRRLeftStage :: [(NumberRowsOpen,[(RowsOpen,[BestRerollChoice])])]
twoRRLeftStage = [ (s,answer s) | s <- [1..13] ]
        -- s is the number of open rows.
  where
    answer s = ([(rowSet,outcomes rowSet) | rowSet <- getChoiceList 13 s ])
              :: [(RowsOpen,[(RollSorted,RerollChoice,Score)])]
      where
        outcomes rowSet = do
          r <- setRolls
          
          -- The set of possible rerolls given r, with their possible outcomes
          -- and probabilities.
          let choicesAndProbs = (getRRProbTable r) 
                :: [(RerollChoice,[RollWithProb])]
          
          -- For each RollSorted in the RollWithProb, refer to
          -- oneRRLeftStage to get the EV, then take the weighted average.
          let stage2EVs = (get1RRLeft s rowSet) :: [(RollSorted,Double)]
          
          -- The set of reroll outcomes in choicesAndProbs for a given 
          -- RerollChoice is limited to those outcomes that actually occur; 
          -- while the set of RollSorted values in stage2EVs includes
          -- all 252 possible forms of 5d6. Look up only the EVs in 
          -- stage2EVs that are relevant for the weighted average.
          let lookupEV aRoll = snd $ fromJust $ find (\(ra,_) -> ra == aRoll) stage2EVs
          let innerFcn = \(theRoll,theProb) -> (theRoll,lookupEV theRoll,theProb)
          let mapFcn = \(rrc,list) -> (rrc, map innerFcn list)
          
          -- Add the ev as another item in the tuple in choicesAndProbs.
          let choicesEVsAndProbs = (map mapFcn choicesAndProbs) 
                :: [(RerollChoice,[(RollSorted,Double,Double)])]
          
          -- For each RerollChoice in choicesEVsAndProbs, we have a set of
          -- reroll outcomes, probabilties and EVs. Convert to a single EV
          -- for each reroll choice. 
          let theChoices = (map (\(rrc,list) -> (rrc,sum $ map (\(_,p,e)->p*e) list)) choicesEVsAndProbs)
                :: [(RerollChoice,Double)]
          
          let (best,ev) = head $ sortOn (negate . snd) theChoices
          
          return (r,best,ev)
        

-- Convenience lookup.
-- 
-- Takes the number of rows open, which rows these are, and returns a list of 
-- all 252 rolls and the EV for that roll when applied to the best selection 
-- among the open rows.
-- 
-- Note that this only returns the part of interest to the EV calculation. 
-- It doesn't include which reroll choice it is that gives the best EV; it
-- merely provides the best EV. The choice of which reroll to take only
-- matters when playing the game, and in that situation you don't really care
-- about the EV since you'll soon have an *actual* value during play.
get2RRLeft :: NumberRowsOpen -> RowsOpen -> [(RollSorted,Double)]
get2RRLeft n sc = extractA layerA n sc
  where
    layerA :: [(RowsOpen,[BestRerollChoice])]
    layerA = snd $ fromJust $ find (\(i,_) -> i==n) twoRRLeftStage
    extractA _ _ _  = extractB layerB sc
      where
        layerB :: [(RollSorted,RerollChoice,Double)]
        layerB = snd $ fromJust $ find (\(s,_) -> s == sc) layerA
        extractB layerBa _ = map (\(a,_,c) -> (a,c)) layerBa


-- Nearly identical to Stage 3/twoRRLeftStage.
oneRRLeftStage :: [(NumberRowsOpen,[(RowsOpen,[BestRerollChoice])])]
oneRRLeftStage = [ (s,answer s) | s <- [1..13] ]
  where
    answer s = [(rowSet,outcomes rowSet) | rowSet <- getChoiceList 13 s ]
      where
        outcomes rowSet = do
          r <- setRolls
          
          let choicesAndProbs = (getRRProbTable r) 
                :: [(RerollChoice,[RollWithProb])]
          
          -- Here's the only meaningful difference: stage 1 instead of stage 2.
          let stage1EVs = (getStage1 s rowSet) :: [(RollSorted,Double)]
          
          let lookupEV aRoll = snd $ fromJust $ find (\(ra,_) -> ra == aRoll) stage1EVs
          let innerFcn = \(theRoll,theProb) -> (theRoll,lookupEV theRoll,theProb)
          let mapFcn = \(rrc,list) -> (rrc, map innerFcn list)
          
          let choicesEVsAndProbs = (map mapFcn choicesAndProbs) 
                :: [(RerollChoice,[(RollSorted,Double,Double)])]
          
          --let innerFcn3 = \(r,p,e) -> p*e
          let innerFcn3 = \(_,p,e) -> p*e
          let theChoices = (map (\(rrc,list) -> (rrc,sum $ map innerFcn3 list)) choicesEVsAndProbs)
                :: [(RerollChoice,Double)]
          
          let (best,ev) = head $ sortOn (negate . snd) theChoices
          
          return (r,best,ev)

-- Convenience lookup.
get1RRLeft :: NumberRowsOpen -> RowsOpen -> [(RollSorted,Double)]
get1RRLeft n sc = extractA layerA n sc
  where
    layerA :: [(RowsOpen,[BestRerollChoice])]
    layerA = snd $ fromJust $ find (\(i,_) -> (i==n)) oneRRLeftStage
    extractA _ _ _ = extractB layerB sc
      where
        layerB :: [(RollSorted,RerollChoice,Double)]
        layerB = snd $ fromJust $ find (\(s,_) -> s == sc) layerA
        extractB layerBa _ = map (\(a,_,c) -> (a,c)) layerBa


-- Aaannnd...Stage 1/rrDoneStage

type BestRowChoice = (RollSorted,Int,Score) -- roll, row choice, and EV.

rrDoneStage :: [(NumberRowsOpen,[(RowsOpen,[BestRowChoice])])]
rrDoneStage = [ (s,answer s) | s <- [1..13] ]
  where
    answer s = [ (rc,outcomes rc) | rc <- getChoiceList 13 s ] :: [(RowsOpen,[BestRowChoice])]
      where
        -- For the given set of open rows and for each of the 252
        -- final rolls, indicate the row chosen and the resulting EV.
        outcomes rc = do
          roll <- setRolls
          
          -- For each open row, determine the sum of the score if r is
          -- used to fill that row *plus* the EV for the remaining unfilled
          -- rows. Choose the row that provides the greatest sum.
          
          -- The list of open rows. Add 1 since we want the row numbers,
          -- not the indices in the list.
          let openRowIndices = map (+1) $ boolToIndex True rc
          
          -- For each of these open rows, see what the score would
          -- be for roll, and the EV for all the *other* open rows.
          let theChoices = [
                (whichRow,
                  getScore whichRow roll,
                  getFinalEV $ swapBoolAt (whichRow-1) rc
                ) | whichRow <- openRowIndices] 
          
          -- Choose the row based on the combined EV:
          let combinedChoices = map (\(row,sa,e) -> (row,fromIntegral(sa)+e)) theChoices
          let (row,score) = maximumBy ( comparing snd ) combinedChoices
          
          return (roll,row,score)

-- Convenient access function.
getStage1 :: Int -> RowsOpen ->[(RollSorted,Double)]
getStage1 n filled = extractA layerA filled :: [(RollSorted,Double)] 
  where
    layerA :: [(RowsOpen,[BestRowChoice])]
    layerA = snd $ fromJust $ find (\(i,_) -> i==n) rrDoneStage
    extractA _ _ = extractB layerB
      where
        layerB :: [BestRowChoice]
        layerB = snd $ fromJust $ find (\(s,_) -> s==filled) layerA
        extractB layerBa = map (\(a,_,c) -> (a,c)) layerBa
        


