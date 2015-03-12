import Data.NGram
import NLP.Crubadan
import NLP.Freq

import System.Directory (getHomeDirectory)

import qualified Data.Text as T
import qualified Data.Set as S

{- For each of these, a file must exist at:
   
   ~/litest/<code>/<code>-chartrigrams.txt
   
   Files in the correct format can be acquired from crubadan.org
   
   -}
testlangs = [ "ab", "en", "es", "fr", "de", "ru", "ja", "ja-Latn"
            , "fub" ]

{- Put the target text at "./testtext.txt" -}
getTargetText = fmap T.pack (readFile "testtext.txt")

main = do paths <- getNGramPaths
          profiles <- sequence (fmap readCrData paths) 
          target <- getTargetText
          let ngs :: [TriGram]
              ngs = ngrams target
              scripts = S.unions (fmap ngblocks ngs) 
              tfreq = freqMap ngs
              scores = fmap (cosine tfreq) profiles
          putStrLn "[[[ Unicode blocks used ]]]"
          print scripts
          putStrLn "[[[ Cosines ]]]"
          sequence_ (fmap print (zip testlangs scores))

getNGramPaths = do home <- getHomeDirectory
                   return (fmap (\l -> home ++ "/litest/" ++ l ++ "/" 
                                       ++ l 
                                       ++ "-chartrigrams.txt") 
                                testlangs)





