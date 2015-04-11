module NLP.Tools.Convenience (smap) where

smap :: (b -> c) -> (a, b) -> (a, c)
smap f (a,b) = (a,f b)
