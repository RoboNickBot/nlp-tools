module NLP.Tools ( choosebest
                 , module NLP.Tools.Database
                 , module NLP.Tools.Convenience ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Applicative

import NLP.General
import NLP.Freq
import NLP.Tools.Database
import NLP.Tools.Convenience

choosebest :: [(String, FreqList TriGram)] -> FreqList TriGram -> String
choosebest fls fl = (bestof . fmap (evaluateT fl)) fls

bestof :: Ord b => [(a,b)] -> a
bestof (f:rs) = fst . foldr (\(a,b) (c,d) -> if b > d
                                                then (a,b)
                                                else (c,d))
                            f $ rs
                            
evaluateT :: (FreqList TriGram) 
          -> (String, FreqList TriGram) 
          -> (String, Double)
evaluateT f (n,p) = (n, cosine f p)
