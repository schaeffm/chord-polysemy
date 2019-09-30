{-# LANGUAGE TypeSynonymInstances #-}
module Identifier where
import Data.DoubleWord
import Data.List (sortBy, nub)

type Identifier = Word256

isBetween :: (Identify a, Identify b, Identify c) => a -> (b,c) -> Bool
isBetween a (begin,end) = 
    if identifier begin == identifier end 
    then identifier a /= identifier begin
    else diff1 < diff2
    where 
        diff1 = identifier end - identifier a
        diff2 = identifier end - identifier begin

isBetweenEnd :: (Identify a, Identify b, Identify c) => a -> (b,c) -> Bool
isBetweenEnd a (begin, end) = identifier a == identifier end || isBetween a (begin, end)

shiftIdentifierLog :: Identify a => a -> Int -> Identifier
shiftIdentifierLog n i = identifier n + 2^(255 - i)

class (Show a, Eq a) => Identify a where
    identifier :: a -> Identifier

instance Identify Identifier where
    identifier = id

smaller :: (Identify a1, Identify a2, Identify b) => b -> a1 -> a2 -> Ordering
smaller ref k1 k2 = 
    if identifier k1 == identifier k2 
    then EQ
    else if identifier k1 `isBetween` (ref, identifier k2)
    then LT
    else GT

sortIdList :: (Identify a, Identify b) => b -> [a] -> [a]
sortIdList ref = sortBy (smaller ref) . nub
