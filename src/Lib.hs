module Lib
    ( mergesort
    ) where

mergesort :: Ord a => [a] -> [a]
mergesort []   = []
mergesort [a]  = [a]
mergesort list = merge (mergesort left) (mergesort right)
  where
    (left, right) = split list [] []
      where
        split [] l r = (l,r)
        split (x:xs) l r = split xs (x:r) l
    merge l [] = l
    merge [] r = r
    merge l@(x:xs) r@(y:ys) = if x <= y
                                then x : merge xs r
                                else y : merge l ys
