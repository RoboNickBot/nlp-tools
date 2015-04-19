import NLP.General
import NLP.Crubadan
import NLP.Freq
import NLP.Tools

import Options.Applicative
import Control.Exception (evaluate)

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S

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
  do db <- connectDB name >>= evaluate
     langs <- fetchLangNames db 
     target <- getContents
     let trFreq :: FreqList TriGram
         trFreq = features target
         
         funs = zip langs $ fmap (\l -> ( (fetchTriGrams db l)
                                        , (fetchLen db l)      )) langs

     scores <- sequence (fmap (check trFreq) funs)
     let niceScores = L.sortBy (\(a,b) (c,d) -> compare b d)
                      . filtNans $ scores
     putStrLn ":: Top Matches ::"
     (sequence_ . fmap print . take num) niceScores

check tr (l,fs) = do score <- cosineM tr fs
                     return (l,score)

rever (a,b) = (b,a)

trs :: String -> FreqList TriGram
trs = read

filtNans :: [(String, Double)] -> [(String, Double)]
filtNans = filter (\(_,d) -> not (isNaN d))
