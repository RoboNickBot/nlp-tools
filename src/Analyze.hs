import NLP.General
import NLP.Crubadan
import NLP.Freq

import NLP.Tools

import Options.Applicative
import System.Directory
import System.IO.Strict
import qualified Data.List as L

percentTestData = 10

data Opts = Opts { opDir :: String
                 , opDB :: String
                 , opPr :: Bool    }

parser = Opts <$> pDir <*> pDB <*> pPr

pDir = strOption (long "source" 
                  <> short 's' 
                  <> metavar "DIRECTORY"
                  <> value "/data/crubadan"
                  <> showDefault
                  <> help h)
  where h = "Root directory of files to use as the test \
            \data (only necessary if \"--use-files\" is \
            \used"
            
pPr = switch (long "use-files"
              <> help h)
  where h = "Use to analyze with a corpora of sample text \
            \on the filesystem, ignoring the database"

pDB = strOption (long "database"
                 <> short 'd'
                 <> metavar "FILENAME"
                 <> value "nlp.db"
                 <> showDefault
                 <> help h)
  where h = "Filename of database to analyze"

desc = fullDesc 
       <> progDesc "Test identifier accuracy using \
                   \pre-classified data" 
       <> header "analyze - test identifier accuracy"

opts = execParser (info (helper <*> parser) desc)

main = opts >>= (\os -> if opPr os
                           then fromFS (opDir os)
                           else fromDB (opDB os))

fromFS :: String -> IO ()
fromFS dir = 
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

fromDB :: String -> IO ()
fromDB name = 
  do db <- connect name
     langs <- getLangNames db
     results <- sequence (fmap (analyze db langs) langs)
     sequence (fmap print results)
     print (stats results)
     
analyze :: IConnection c => c -> [String] -> String -> IO (String,String)
analyze db langs lang = 
  do ltest <- smap read <$> fetchLangTestData db lang
     trigs <- fmap (smap read) 
              <$> sequence (fmap (fetchLangMainData db) langs)
     let winner = choosebest trigs (snd ltest)
     return (lang, winner)

stats :: [(String,String)] -> String
stats ss = let total = length ss
               correct = length (filter (\(a,b) -> a == b) ss)
               percent = correct * 100 `div` total
           in "Accuracy: "
              ++ (show correct) 
              ++ " / " 
              ++ (show total)
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
