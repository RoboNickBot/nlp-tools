import NLP.General
import NLP.Crubadan
import NLP.Freq

import NLP.Tools

import Control.Exception (evaluate)
import System.IO (hPutStrLn, stderr)
import Options.Applicative
import Control.Monad
import System.Directory
import System.IO.Strict
import qualified Data.List as L
import qualified Data.Map.Strict as M

percentTestData = 10

data Opts = Opts { opDir :: String
                 , opDB :: String
                 , opPr :: Bool
                 , opNum :: Int
                 , opNumTrigrams :: Int }

parser = Opts <$> pDir <*> pDB <*> pPr <*> pNum <*> pTrigrams

pTrigrams = option auto (long "num-trigrams"
                         <> short 't'
                         <> value 5000
                         <> metavar "NUMBER"
                         <> showDefault
                         <> help h)
  where h = "The number of trigrams to pull from each checked \
            \language for the comparison (starting with the \
            \most frequent)"

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
                           else fromDB os)

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

fromDB :: Opts -> IO ()
fromDB os = 
  do db <- connectDB (opDB os)
     langs <- (fmap fst . M.toList) 
              <$> fetchAllLengths db "dataAll"
     let sets = divie' "   " (opNum os) langs
     st <- fetchSt db (opNum os)
     let crawl1' = crawl1 st os sets
     results <- foldM crawl1' M.empty sets
     
     let winners = fmap (snd . maximum) results
     (sequence_ . fmap print . M.toList) winners
     (putStrLn . stats . M.toList) winners

crawl1 :: Statement -> Opts -> [[String]] -> Results -> [String] -> IO Results
crawl1 st os sets results f1 = 
  do trs <- fetch st "dataA" (opNumTrigrams os) f1
     foldM (crawl2 st os trs) results sets

crawl2 :: Statement -> Opts -> M.Map String (FreqList TriGram) -> Results -> [String] -> IO Results
crawl2 st os dataA results f2 =
  do trs <- fetch st "dataB" (opNumTrigrams os) f2
     hPutStrLn stderr ("\nNow crunching\n" ++ (show $ M.keys dataA) ++ "\nand\n" ++ (show f2))
     evaluate (L.foldl' (check os (M.toList trs)) results (M.toList dataA))

check :: Opts -> [(String, FreqList TriGram)] -> Results -> (String, FreqList TriGram) -> Results
check os dataB results a =
  L.foldl' (compTwo os a) results dataB

compTwo :: Opts -> Lang -> Results -> Lang -> Results
compTwo os (la,fa) r (lb,fb) = mupd la lb (cosine fa fb) r

mupd :: String -> String -> Double -> Results -> Results
mupd tn cn v r = case M.lookup tn r of
                   Just vs -> M.insert tn ((v,cn) : vs) r
                   Nothing -> M.insert tn ((v,cn) : []) r

-- combineRs :: [String] -> [Results] -> Results
-- combineRs ls rs = 
--   foldr (\k -> M.insert k (mid k rs)) M.empty ls
-- 
-- mid :: String -> [Results] -> [(Double,String)]
-- mid k rs = foldr cc [] (fmap (M.lookup k) rs)
-- 
-- cc :: Maybe [a] -> [a] -> [a]
-- cc (Just as) bs = as ++ bs
-- cc _ bs = bs

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

-- analyze :: Database -> [String] -> [String] -> IO Results
-- analyze db langs cands = 
--   do cs <- manyData db fetchBData cands
--      ts <- manyData db fetchAData langs
-- 
--      return (foldr (compAll ts) M.empty cs)
-- 
-- compAll :: [Lang] -> Lang -> Results -> Results
-- compAll ts (cn,cd) r = 
--   foldr (\(tn,td) -> mupd tn cn (cosine td cd)) r ts
--   
-- mupd :: String -> String -> Double -> Results -> Results
-- mupd tn cn v r = case M.lookup tn r of
--                    Just vs -> M.insert tn ((v,cn) : vs) r
--                    Nothing -> M.insert tn ((v,cn) : []) r
-- 
-- manyData db f ns = sequence (fmap (f db) ns)
-- 
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
