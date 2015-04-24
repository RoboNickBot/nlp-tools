import NLP.General
import NLP.Crubadan
import NLP.Freq
import NLP.Tools

import Options.Applicative
import Control.Exception (evaluate)
import Control.Monad

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

data Opts = Opts String Int

pNum = option auto (long "num-results"
                    <> short 'n'
                    <> value 10000
                    <> metavar "NUMBER"
                    <> showDefault
                    <> help h)
  where h = "Maximum number of top results to print"

pDB = strOption (long "database" 
                 <> short 'd'
                 <> value "nlp.db"
                 <> metavar "FILENAME"
                 <> showDefault
                 <> help h )
  where h = "Database to use for identification"
  
desc = fullDesc
       <> progDesc "Identify text on standard input as \
                   \a particular language, using language \
                   \profiles stored in a database \
                   \(see builddb)"
       <> header "identify the language of a text"
       
parser = Opts <$> pDB <*> pNum

execOpts = execParser (info (helper <*> parser) desc)



main = execOpts >>= identify

identify (Opts name num) = 
  do db <- connectDB name
     target <- getContents
     candidates <- (fmap fst . M.toList) 
                   <$> fetchAllLengths db "dataAll" 
     let trFreq :: FreqList TriGram
         trFreq = features target
         grams = M.keys (freqMap trFreq)
     st <- fetchSt db grams
     let check' = check st 30 trFreq grams "dataAll"
     scores <- foldM check' [] candidates
     putStrLn "\n:: Top Matches ::"
     (sequence_ . fmap print) scores

check st num frq grams set scores lang = 
  do putStrLn ("Evaluating " ++ lang ++ "...")
     putStrLn ("With " ++ show (length grams) ++ " trigrams...")
     op <- fetch st set lang grams
     let score = (lang, cosine frq op)
     -- putStrLn ("With " ++ show op) -- show freqlists (noisy)
     evaluate ((take num . scoreSort) (score : scores))

scoreSort :: [(String, Double)] -> [(String, Double)]
scoreSort = L.sortBy (\(a,x) (b,y) -> compare y x)

rever (a,b) = (b,a)

trs :: String -> FreqList TriGram
trs = read

filtNans :: [(String, Double)] -> [(String, Double)]
filtNans = filter (\(_,d) -> not (isNaN d))
 
         
         
--funs = zip langs $ fmap (\l -> ( (fetchTriGrams db l)
--                               , (fetchLen db l)      )) langs
