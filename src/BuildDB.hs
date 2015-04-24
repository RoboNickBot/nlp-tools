import System.Directory
import System.IO (hPutStrLn, stderr)
import Options.Applicative
import qualified Data.List as L
import qualified Data.Map.Strict as M
-- import qualified System.IO.Strict as St

import NLP.General
import NLP.Freq
import NLP.Tools

percent :: Parser Int
percent = option auto (long "percent" 
                       <> short 'p'
                       <> metavar "PERCENT"
                       <> value 10
                       <> showDefault
                       <> help h)
  where h = "Percentage of text to be withheld for testing, as \
            \a number in the range [0,100]" 

dbname :: Parser String
dbname = strOption (long "output" 
                    <> short 'o'
                    <> metavar "FILENAME"
                    <> value "nlp.db"
                    <> showDefault
                    <> help h)
  where h = "Filename of the database to be built"

rootdir :: Parser String
rootdir = strOption (long "source"
                     <> short 's'
                     <> metavar "DIRECTORY"
                     <> value "/data/crubadan"
                     <> showDefault
                     <> help h)
  where h = "Root directory of source language data"

desc = fullDesc
       <> progDesc "Build database for NLP Tools from \
                   \language data on the filesystem"
       <> header "builddb - build nlp database"

data Opts = Opts String String Int

parser = Opts <$> dbname <*> rootdir <*> percent

execOpts = execParser (info (helper <*> parser) desc)

main = execOpts >>= mkdatabase

mkdatabase (Opts dbname dataroot prc) = 
  do dirs <- datadirs dataroot
     let files = datafilenames dataroot dirs
     
     createDB dbname
     db <- connectDB dbname

     sequence_ (fmap (processFile db prc) files)

     disconnectDB db

processFile :: Database -> Int -> (String, String) -> IO ()
processFile db p (l,fn) = 
  do sents <- datafile (l,fn)
     let (aSents, bSents) = splitLang p sents
         f = smap ftrig
         allData = f sents
         aData = f aSents
         bData = f bSents
     store db "dataAll" allData
     store db "dataA" aData
     store db "dataB" bData
     hPutStrLn stderr $ "Inserted lang <" ++ l ++ "> ..."
     
store :: Database -> String -> (String, FreqList TriGram) -> IO ()
store db set d = do insertTriGrams db (mkTGRows set d)
                    insertLengths db [(mkLRow set (smap len d))]

mkTGRows :: String 
         -> (String, FreqList TriGram) 
         -> [(String,String,TriGram,Int,Int)]
mkTGRows set (lang,fl) = let toRow ((tg,fr),n) = (set,lang,tg,fr,n)
                         in fmap toRow (zip ((tSort . M.toList) 
                                               (freqMap fl)) [0,1..])

tSort = L.sortBy (\(l1,f1) (l2,f2) -> compare f2 f1)

mkLRow :: String -> (String, Double) -> (String, String, Double)
mkLRow set (lang,len) = (set,lang,len)

datafilenames :: String -> [String] -> [(String,String)]
datafilenames root dirs = 
  fmap (\n -> (n, root ++ "/" ++ n ++ "/SAMPSENTS")) dirs

datadirs = fmap (L.delete ".") 
           . fmap (L.delete "..") 
           . getDirectoryContents

datafile :: (String, String) -> IO (String, String)
datafile (lang,fn) = (,) lang <$> readFile fn

ftrig :: String -> FreqList TriGram
ftrig = features

splitLang :: Int 
          -> (String, String) 
          -> ((String, String), (String, String))
splitLang p (l,d) = let dls = lines d
                        i = ((length dls) * p) `div` 100
                        (d1,d2) = splitAt i dls
                    in ( (l, concat d1)
                       , (l, concat d2))
