module NLP.Tools (choosebest) where

import NLP.General
import NLP.Freq

choosebest :: [(String, FreqList TriGram)] -> FreqList TriGram -> String
choosebest fls fl = (bestof . fmap (evaluate fl)) fls

bestof :: Ord b => [(a,b)] -> a
bestof (f:rs) = fst . foldr (\(a,b) (c,d) -> if b > d
                                                then (a,b)
                                                else (c,d))
                            f $ rs
                            
evaluate :: (FreqList TriGram) 
         -> (String, FreqList TriGram) 
         -> (String, Double)
evaluate f (n,p) = (n, cosine f p)
