import NLP.General
import NLP.Crubadan
import NLP.Freq
import NLP.Tools

import Options.Applicative

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S

data Opts = Opts String

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
       
parser = Opts <$> pDB

execOpts = execParser (info (helper <*> parser) desc)



main = execOpts >>= identify

identify (Opts name) = 
  do db <- connect name
     langs <- getLangNames db
     datas <- fmap (smap trs) 
              <$> sequence (fmap (fetchLangMainData db) langs)
     target <- getContents
     let trFreq :: FreqList TriGram
         trFreq = features target

         scores = L.reverse
                  . L.sort 
                  . fmap rever 
                  . fmap (smap (cosine trFreq)) $ datas 
     putStrLn ":: Top Matches ::"
     (sequence_ . fmap print . take 10) scores

rever (a,b) = (b,a)

trs :: String -> FreqList TriGram
trs = read
