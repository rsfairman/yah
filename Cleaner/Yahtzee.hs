module Yahtzee where

{-

NOTE: This version parallelizes badly, and it's unclear why. Here's a guess.
The 'Initial' version does parallize well. One of the mysteries about 
'Initial' is the fact that it works at all. My guess is that the compiler,
being unable to figure out exactly which bits of the tables in 'Initial' 
depend on which other bits (due to the use of 'find' and '!!'), chooses to 
reduce much of the calculation to normal form (fully evaluates it). In
contrast, this version does the calculation within a single table; this
allows the compiler to *think* that it knows the best way to sequence the
calculation, but it's wrong.

---------------------------------------------------------------------------

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
import Control.Parallel.Strategies (parList,rdeepseq,rpar,rparWith,rseq,runEval,using,parEval)
import Control.DeepSeq(force,deepseq)
import Control.Parallel(pseq)

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
--
-- In this version, there is only one table (i.e., one Haskell value).
-- Each "outer entry" of the list making up the table corresponds to
-- some number of open rows in the scorecard. So, the i-th entry of
-- monster contains the data when there are i rows open on the scorecard.
-- Each of these entries is itself a list, consisting of tuples, with one
-- item in the tuple for each "stage" of the calculation described above,
-- and indexed by the various choices for choosing i open rows.

type RowsOpen = [Bool] -- True ==> row open.
type EV = Double
type RowChoice = Int -- in [1..13]

-- Under different scenarios (implicit from the context where these types
-- are used), these are the various choices for what the player might do,
-- and the EVs for those choices.
--
-- RowOptions indicates, for each possible RollSorted, the best row to
-- choose and the EV of that choice. RerollOptions is similar, but it 
-- indicates the best choice for which dice to reroll.
type RowOptions = [(RollSorted,RowChoice,EV)]
type RerollOptions = [(RollSorted,RerollChoice,EV)]

monster :: [
        [(
        RowsOpen,      -- Must match the current index of the outer list
        EV,            -- finalEV 
        RowOptions,    -- rrDoneStage
        RerollOptions, -- oneRRLeftStage
        RerollOptions  -- twoRRLeftStage
        )]
        ]

monster = -- Very bad place to parallelize
          -- [ entry i | i <- [0..13] ] `using` parList rdeepseq
      [ entry i | i <- [0..13] ]
  where
    entry :: Int -> [(RowsOpen,EV,RowOptions,RerollOptions,RerollOptions)]
    
    -- Base case of recursion: no rows open.
    entry 0 = [(replicate 13 False,0,[],[],[])]
    
    -- Other entries depend on the previous finalEV stage (and nothing else).
    -- Note that parallelizing here works, in the sense that all cores are
    -- active and used at near 100%, but it's clearly not efficient.
    entry n = [ (rowSet,finalEV rowSet,rrDone rowSet,oneRRLeft rowSet,twoRRLeft rowSet) | rowSet <- getChoiceList 13 n] `using` parList rdeepseq
      where
          -- I tried this, forcing the previous entry to be fully evaluated
          -- before proceeding, but it doesn't parallelize any better.
          -- prevFinal = (entry (n-1)) `pseq` getFinalEVNEW $ entry (n-1)
          prevFinal = getFinalEVNEW $ entry (n-1)
          
          rrDone rowSet = (getRRDoneNEW prevFinal rowSet) :: RowOptions
          
          -- The row choices from rrDone would important when playing the
          -- game, but not needed here.
          doneStripped rowSet = map (\(roll,_,ev) -> (roll,ev)) (rrDone rowSet)
          oneRRLeft rowSet = getRRChoicesNEW (doneStripped rowSet)
          
          -- Same thing for twoRRLeft.
          oneRRStripped rowSet = map (\(roll,_,ev) -> (roll,ev)) (oneRRLeft rowSet)
          twoRRLeft rowSet = getRRChoicesNEW (oneRRStripped rowSet)
          
          -- Again, but this layer is simple, so do it here.
          twoRRStripped rowSet = map (\(roll,_,ev) -> (roll,ev)) (twoRRLeft rowSet)
          finalEV rowSet = weightedEVOfRolls theRollProbs (twoRRStripped rowSet)

-- HERE'S THE STRANGE THING. THE CODE BELOW SHOULD BE IDENTICAL -- it's
-- been rearranged a bit to read better, using do-notation, and that's the
-- only difference. But it won't parallelize. No matter what I've tried,
-- it compiles and runs, but only seems to use one core.

-- MAYBE WHAT I SHOULD DO HERE IS MAKE THE go FUNCTION run in parallel
    
    -- -- Only uses one core.
    -- -- entry n = go ( getFinalEVNEW ( entry (n-1) ) ) `using` parList rdeepseq
    -- entry n = go ( getFinalEVNEW ( entry (n-1) ) )
    --   where
    --     -- Doesn't do it either -- no surprise since it should do the same
    --     -- thing as the other attempts.
    --     -- go prevFinal = (`using` parList rdeepseq) $ do 
    --     go prevFinal = do 
    --       rowSet <- (getChoiceList 13 n )
          
    --       let rrDone = (getRRDoneNEW prevFinal rowSet) :: RowOptions
          
    --       -- The row choices from rrDone would important when playing the
    --       -- game, but not needed here.
    --       let doneStripped = map (\(roll,_,ev) -> (roll,ev)) rrDone
    --       let oneRRLeft = getRRChoicesNEW doneStripped
          
    --       -- Same thing for twoRRLeft.
    --       let oneRRStripped = map (\(roll,_,ev) -> (roll,ev)) oneRRLeft
    --       let twoRRLeft = getRRChoicesNEW oneRRStripped
          
    --       -- Again, but this layer is simple, so do it here.
    --       let twoRRStripped = map (\(roll,_,ev) -> (roll,ev)) twoRRLeft
    --       let finalEV = weightedEVOfRolls theRollProbs twoRRStripped
          
    --       -- Using pseq here should (?) force finalEV to be evaluated, but
    --       -- that doesn't help.
    --       -- I also wrapped the entire do-block in parenthisis and did this,
    --       -- but no different than putting the `using` up with the call to 
    --       -- go (as one would expect).
    --       -- finalEV `pseq` return (rowSet,finalEV,rrDone,oneRRLeft,twoRRLeft) ) `using` parList rdeepseq
    --       finalEV `pseq` return (rowSet,finalEV,rrDone,oneRRLeft,twoRRLeft)



-- The functions that follow could have been defined within the monster
-- calculation, but pulling them out makes things tidier and easier to
-- understand.


-- Given one entry of monster, pull out the finalEV part of the data.
getFinalEVNEW :: [(RowsOpen,EV,RowOptions,RerollOptions,RerollOptions)] -> [(RowsOpen,EV)]
getFinalEVNEW entireRow = map (\(opens,ev,_,_,_) -> (opens,ev)) entireRow

-- Convenience lookup. SLOW!
findFinal :: [(RowsOpen,EV)] -> RowsOpen -> EV
findFinal list which = snd $ fromJust $ find (\(possible,_) -> possible == which) list


-- Takes the EVs from the finalEV stage, together with a particular choice
-- of open rows, and returns the best row to choose for each roll and the EV.
getRRDoneNEW :: [(RowsOpen,EV)] -> RowsOpen -> RowOptions
getRRDoneNEW prevFinal rowsOpen = do
    roll <- setRolls
    
    -- For each open row in rowsOpen, determine the sum of the score if roll 
    -- is used to fill that row *plus* the EV for the remaining unfilled
    -- rows. Choose the row that provides the greatest sum.
    
    -- The list of open rows. Add 1 since we want the row numbers,
    -- not the indices in the list.
    let openRowIndices = map (+1) $ boolToIndex True rowsOpen
    
    -- For each of these open rows, see what the score would
    -- be for roll, and the EV for all the *other* open rows.
    let theChoices = [
          (whichRow,
            getScore whichRow roll,
            findFinal prevFinal (swapBoolAt (whichRow-1) rowsOpen)
          ) | whichRow <- openRowIndices] 
    
    -- Choose the row based on the combined EV:
    let combinedChoices = map (\(row,sa,e) -> (row,fromIntegral(sa)+e)) theChoices
    let (row,score) = maximumBy ( comparing snd ) combinedChoices
    
    return (roll,row,score)


getRRChoicesNEW :: [(RollSorted,EV)] -> RerollOptions
getRRChoicesNEW prevEVs = do
    r <- setRolls
    
    -- The set of possible rerolls given r, with their possible outcomes
    -- and probabilities.
    let choicesAndProbs = (getRRProbTable r) :: [(RerollChoice,[RollWithProb])]
    
    -- For each RollSorted in a RollWithProb, refer to prevEVs and take the
    -- weighted average. Remember: the set of reroll outcomes in
    -- choicesAndProbs for a given RerollChoice is limited to those outcomes
    -- that actually occur, while the set of RollSorted values in prevEVs 
    -- includes all 252 possible forms of 5d6.
    let lookupEV aRoll = snd $ fromJust $ find (\(ra,_) -> ra == aRoll) prevEVs
    let innerFcn = \(theRoll,theProb) -> (theRoll,lookupEV theRoll,theProb)
    let mapFcn = \(rrc,list) -> (rrc,map innerFcn list)
    
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


