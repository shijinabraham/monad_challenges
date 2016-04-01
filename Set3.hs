{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}


module Set3 where

import MCPrelude


allPairs :: [a] -> [b] -> [(a,b)]
{-allPairs (a:as) bs = (map (\b-> (a,b)) bs)++(allPairs as bs)
allPairs [] _ = []-}
allPairs = allComb (,)


--allPairs cardRanks cardSuits == [(2,"H"),(2,"D"),(2,"C"),(2,"S"),(3,"H"),(3,"D"),(3,"C"),(3,"S"),(4,"H"),(4,"D"),(4,"C"),(4,"S"),(5,"H"),(5,"D"),(5,"C"),(5,"S")]


data Card = Card {r::Int, s::[Char]}

instance Show Card where
  show (Card r s)  = (show r) ++  s


allCards :: [Int] -> [String] -> [Card]
{-
allCards (r:rs) s = (map (\s'-> Card r s') s) ++ (allCards rs s)
allCards [] _ = []
-}
allCards = allComb Card

--show (allCards cardRanks cardSuits) == "[2H,2D,2C,2S,3H,3D,3C,3S,4H,4D,4C,4S,5H,5D,5C,5S]"

allComb :: (a->b->c) -> [a] -> [b] -> [c]
{-allComb f (a:as) b = (map (\b'-> f a b') b)++(allComb f as b)
allComb _ [] _ = []
-}
allComb f a b = combStep (map f a) b


allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
{-allCombs3 f a b c  = let fl = allComb f a b
                      in allComb ($) fl c-}
allCombs3 f a b c = combStep (combStep (map f a) b) c


--allCombs3 (,,) [1,2] [3,4] [5,6] == [(1,3,5),(1,3,6),(1,4,5),(1,4,6),(2,3,5),(2,3,6),(2,4,5),(2,4,6)]

combStep :: [a->b] -> [a] -> [b]
combStep (f:fs) a = (map (\a' -> (f a')) a) ++ (combStep fs a)
combStep [] _ = []
  
