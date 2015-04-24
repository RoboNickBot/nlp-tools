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

data Opts = Opts String Int Int

pNumPer = option auto (long "num-per-read"
                       <> short 'p'
                       <> value 20
                       <> metavar "NUMBER"
                       <> showDefault
                       <> help h)
  where h = "The number of frequency-lists to read from the \
            \database at a single time -- A larger value will \
            \make the program run faster, but at a higher \
            \memory cost."

pNumRes = option auto (long "num-results"
                       <> short 'n'
                       <> value 50
                       <> metavar "NUMBER"
                       <> showDefault
                       <> help h)
  where h = "Maximum number of top results to present"

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
       
parser = Opts <$> pDB <*> pNumRes <*> pNumPer

execOpts = execParser (info (helper <*> parser) desc)



main = execOpts >>= identify

identify (Opts name numResults numPer) = 
  do db <- connectDB name
     target <- getContents
     candidates <- (fmap fst . M.toList) 
                   <$> fetchAllLengths db "dataAll" 
     let trFreq :: FreqList TriGram
         trFreq = features target
     st <- fetchSt db numPer
     let crawl' = crawl st numResults trFreq "dataAll"
     scores <- foldM crawl' [] (divie' "   " numPer candidates)
     putStrLn "\n:: Top Matches ::"
     (sequence_ . fmap print) scores

crawl st numResults frq set scores langs = 
  do (sequence_ 
      . fmap putStrLn 
      . fmap (\l -> "Evaluating " ++ l ++ "...")) langs
     ops <- fetch st set langs
     evaluate (foldl (check numResults frq) scores (M.toList ops))

check :: Int 
      -> FreqList TriGram 
      -> [(String, Double)] 
      -> (String, FreqList TriGram) 
      -> [(String, Double)]
check numResults freq scores (lang,op) = 
  let score = (lang, cosine freq op)
  in (take numResults . scoreSort) (score : scores)

scoreSort :: [(String, Double)] -> [(String, Double)]
scoreSort = L.sortBy (\(a,x) (b,y) -> compare y x)

rever (a,b) = (b,a)

trs :: String -> FreqList TriGram
trs = read

filtNans :: [(String, Double)] -> [(String, Double)]
filtNans = filter (\(_,d) -> not (isNaN d))

divie :: Int -> [a] -> [[a]]
divie 0 _ = [[]] -- possibly surprising?
divie _ [] = []
divie n xs = let (as,bs) = splitAt n xs
             in as : divie n bs
             
divie' :: a -> Int -> [a] -> [[a]]
divie' _ 0 _ = [[]]
divie' _ _ [] = []
divie' p n xs = let (as,bs) = splitAt n xs
                    l = length as
                in (as ++ replicate (n - l) p) : divie' p n bs
