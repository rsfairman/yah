module YahRules(getScore) where

{-

This encodes the rules of a game similar to Yahtzee, but simpler. The function
fillRow encodes these rules.

The two differences of this game relative to the actual game are as follows.
(1) No 35 point bonus in the top half. Under the ordinary rules, if the scores
    in the top six rows sum to 63 or more, then the player gets an additional
    35 points.
(2) There is no 100 point bonus for extra yahtzees (five-of-a-kind), and a
    yahtzee can be used for any row, without limitation. Under the ordinary 
    "forced choice" joker rule, an additional yahtzee must be used to fill the
    corresponding top row if that top row is open. Some people play using the 
    "free choice" joker rule, under which the player is not required to fill 
    the corresponding top row first, but the player may not use the yahtzee as
    a joker to fill the full house or straight rows until the top row has
    been filled. Under the rules used here, there are no restrictions on using
    a yahtzee as a joker. This might be called the "unlimited" joker rule.
    
-}

import qualified Data.Vector as Vb
import qualified Data.Vector.Unboxed as Vu

import YahDice (RollSorted,RollIndex,toRollSorted,setRolls)



-- This function is the main purpose of this module.
-- The first argument is the row to be filled (1-13), and the marginal score 
-- is returned.
fillRow :: Int -> RollSorted ->  Int

-- Sum of ones, twos, etc. 
fillRow 1 r = sum [ v | v<-r, v == 1 ]
fillRow 2 r = sum [ v | v<-r, v == 2 ]
fillRow 3 r = sum [ v | v<-r, v == 3 ]
fillRow 4 r = sum [ v | v<-r, v == 4 ]
fillRow 5 r = sum [ v | v<-r, v == 5 ]
fillRow 6 r = sum [ v | v<-r, v == 6 ]

-- Three-of-a-Kind
-- If there *is* a three-of-a-kind, then take the sum of *all* dice.
-- Remember, the roll is sorted.
fillRow 7 [r1,r2,r3,r4,r5]
  | (r1 == r2) && (r2 == r3) = r1+r2+r3+r4+r5
  | (r2 == r3) && (r3 == r4) = r1+r2+r3+r4+r5
  | (r3 == r4) && (r4 == r5) = r1+r2+r3+r4+r5
  | otherwise = 0
        
-- Four-of-a-Kind
fillRow 8 [r1,r2,r3,r4,r5]
  | (r1 == r2) && (r2 == r3) && (r3 == r4) = r1+r2+r3+r4+r5
  | (r2 == r3) && (r3 == r4) && (r4 == r5) = r1+r2+r3+r4+r5
  | otherwise = 0

-- Full House.
-- Not that a five-of-a-kind is also a full house under this definition.
fillRow 9 [r1,r2,r3,r4,r5]
  | (r1 == r2) && (r3 == r4) && (r4 == r5) = 25
  | (r1 == r2) && (r2 == r3) && (r4 == r5) = 25
  | otherwise = 0

-- Small Straight
fillRow 10 [r1,r2,r3,r4,r5]
  | (r2 == r1+1) && (r3 == r2+1) && (r4 == r3+1) = 30
  | (r3 == r2+1) && (r4 == r3+1) && (r5 == r4+1) = 30
  | otherwise = 0

-- Large Straight
fillRow 11 [r1,r2,r3,r4,r5]
  | (r2 == r1+1) && (r3 == r2+1) && (r4 == r3+1) && (r5 == r4+1) = 40
  | otherwise = 0

-- Yahtzee
fillRow 12 [r1,r2,r3,r4,r5]
  | (r1 == r2) && (r2 == r3) && (r3 == r4) && (r4 == r5) = 50
  | otherwise = 0;

-- Chance
fillRow 13 r = sum r

fillRow _ _ = error "What game are *you* playing?"



------------------------------------------------------
-- For efficiency, generate a big table with every possible way of filling a
-- single row.
--
-- As of 'RollVector', this uses vectors for faster lookup.
type RowScore = Int

-- The scoreTable is boxed vector, one entry for each roll index, of
-- scores for the different rows. So it's size is 252 by 13.
-- It's possible that indexing the other way (row outside, roll inside)
-- would be faster, but haven't checked. It seems unlikely, given how
-- this is used in Yahtzee.hs.
scoreTable :: Vb.Vector (Vu.Vector RowScore) 
scoreTable = Vb.fromList [ Vu.fromList [ fillRow s (toRollSorted r) | s <- [1..13] ] | r <- setRolls ]


-- Convenience lookup. Provide the row number (1-13), the roll and obtain 
-- the score. We can now use ! instead of !! (!).
getScore :: Int -> RollIndex -> Int
getScore row roll = ( scoreTable Vb.! roll ) Vu.! (row - 1)




