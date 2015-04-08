import NLP.General
import NLP.Crubadan
import NLP.Freq

import NLP.Tools

import Options.Applicative
import System.Directory
import System.IO.Strict
import qualified Data.List as L

percentTestData = 10

rdir :: Parser String
rdir = strOption ( long "directory" 
                <> short 'd' 
                <> metavar "DIRECTORY" 
                <> help "Root directory of data files")

opts = info (helper <*> rdir) ( fullDesc 
                             <> progDesc "Test identifier"
                             <> header "analyze - test identifier")

main = execParser opts >>= testall

testall dir = 
  do targets <- (fmap (L.delete ".") . fmap (L.delete "..") 
                 . getDirectoryContents) dir
     texts <- sequence (fmap (bfetch . qual dir) targets)
     let (ts,ps) = testprocess texts

         tf :: [(String, FreqList TriGram)]
         tf = fmap (\(a,b) -> (a,features b)) ts

         pf :: [(String, FreqList TriGram)]
         pf = fmap (\(a,b) -> (a,features b)) ps

         res :: [(String, String)]
         res = fmap (\(n,f) -> (n, choosebest pf f)) tf

     sequence_ (fmap print res)
     putStrLn (stats res)

stats :: [(String,String)] -> String
stats ss = let total = length ss
               correct = length (filter (\(a,b) -> a == b) ss)
               percent = (fromIntegral correct)
                         / (fromIntegral total)
                         * 100
           in "Accuracy: "
              ++ (show total) 
              ++ " / " 
              ++ (show correct)
              ++ " ("
              ++ (show percent)
              ++ ("%)")

qual r s = (s, r ++ "/" ++ s ++ "/SAMPSENTS")

bfetch (n,p) = do t <- System.IO.Strict.readFile p 
                  return (n,t)


testprocess :: [(String,String)] -> ([(String, String)], [(String, String)])
testprocess = (\ts -> (fmap fst ts, fmap snd ts)) . fmap d

d (id,text) = let ls = parseSampSents text
                  num = length ls
                  (t,p) = L.splitAt 
                               (num * percentTestData `div` 100) ls
              in ((id, concat t), (id, concat p))


parseSampSents :: String -> [String]
parseSampSents = lines
