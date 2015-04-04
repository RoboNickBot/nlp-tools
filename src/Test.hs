import NLP.General
import NLP.Crubadan
import NLP.Freq

import System.Directory
import qualified Data.List as L

percentTestData = 10

main = do targets <- getDirectoryContents "/data/crubadan"
          texts <- sequence (fmap qual targets)
          let (ts,ps) = testprocess texts
          return ()
          
qual s = do t <- readFile ("/data/crubadan/" ++ s ++ "/SAMPSENTS")
            return (s,t)

testprocess :: [(String,String)] -> ([(String, String)], [(String, String)])
testprocess = (\ts -> (fmap fst ts, fmap snd ts)) fmap d

d (id,text) = let ls = parseSampSents text
                  num = length ls
                  (t,p) = L.splitAt 
                               (num * 100 / percentTestData) ls
              in ((id, concat t), (id, concat p))

parseSampSents :: String -> [String]
parseSampSents = lines
