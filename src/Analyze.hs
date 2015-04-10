import NLP.General
import NLP.Crubadan
import NLP.Freq

import NLP.Tools

import Options.Applicative
import System.Directory
import System.IO.Strict
import qualified Data.List as L
import qualified Data.Map as M

percentTestData = 10

data Opts = Opts { opDir :: String
                 , opDB :: String
                 , opPr :: Bool
                 , opNum :: Int    }

parser = Opts <$> pDir <*> pDB <*> pPr <*> pNum

pNum = option auto (long "load-size"
                    <> short 'n'
                    <> metavar "NUMBER"
                    <> value 20
                    <> showDefault
                    <> help h)
  where h = "The number of languages to load at one time; \
            \a higher number will speed up execution but\
            \will also consume more memory..."

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
                           else fromDB (opDB os, opNum os))

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

type Results = M.Map String [(Double, String)]
type Lang = (String, FreqList TriGram)

fromDB :: (String, Int) -> IO ()
fromDB (name, num) = 
  do db <- connect name
     langs <- getLangNames db
     let sets = divie num langs
     r <- sequence (fmap (analyze db langs) sets)
     let results = combineRs langs r
         winners = fmap (snd . maximum) results
     (sequence_ . fmap print . M.toList) winners
     (putStrLn . stats . M.toList) winners

combineRs :: [String] -> [Results] -> Results
combineRs ls rs = 
  foldr (\k -> M.insert k (mid k rs)) M.empty ls

mid :: String -> [Results] -> [(Double,String)]
mid k rs = foldr cc [] (fmap (M.lookup k) rs)

cc :: Maybe [a] -> [a] -> [a]
cc (Just as) bs = as ++ bs
cc _ bs = bs

divie :: Int -> [a] -> [[a]]
divie 0 _ = [[]] -- possibly surprising?
divie _ [] = []
divie n xs = let (as,bs) = splitAt n xs
             in as : divie n bs

analyze :: IConnection c => c -> [String] -> [String] -> IO Results
analyze db langs cands = 
  do cs <- manyData db fetchLangMainData cands
     ts <- manyData db fetchLangTestData langs
     
     return (foldr (compAll ts) M.empty cs)

compAll :: [Lang] -> Lang -> Results -> Results
compAll ts (cn,cd) r = 
  foldr (\(tn,td) -> mupd tn cn (cosine td cd)) r ts
  
mupd :: String -> String -> Double -> Results -> Results
mupd tn cn v r = case M.lookup tn r of
                   Just vs -> M.insert tn ((v,cn) : vs) r
                   Nothing -> M.insert tn ((v,cn) : []) r

manyData db f ns = fmap (smap read) <$> sequence (fmap (f db) ns)

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
