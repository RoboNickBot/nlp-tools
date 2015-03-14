import NLP.General
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
          let toks :: [NGToken]
              toks = tokens target

              trFreq :: FreqList (TriGram NGToken)
              trFreq = ( metaFeatures 
                       . (features :: [NGToken] -> [TriGram NGToken])
                       ) toks

              sFreq :: FreqList (UBlock)
              sFreq = ( metaFeatures 
                      . (features :: [NGToken] -> [UBlock])
                      ) toks 

              scores = fmap (cosine trFreq) profiles

          putStrLn "[[[ Unicode blocks used ]]]\n"
          sequence_ (fmap putStrLn (prettyprint sFreq))
          putStrLn "\n[[[ Top 10 Character TriGrams ]]]\n"
          sequence_ (fmap putStrLn (take 10 (prettyprint trFreq)))
          putStrLn "\n[[[ Cosines ]]]\n"
          sequence_ (fmap print (zip testlangs scores))

getNGramPaths = do home <- getHomeDirectory
                   return (fmap (\l -> home ++ "/litest/" ++ l ++ "/" 
                                       ++ l 
                                       ++ "-chartrigrams.txt") 
                                testlangs)





