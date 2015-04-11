import System.Directory
import System.IO (hPutStrLn, stderr)
import Options.Applicative
import qualified Data.List as L

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
     sents <- sequence (fmap datafile files)
     let (aSents,bSents) = 
           unzip (fmap (splitLang prc) sents)
         f = fmap (smap ftrig)
         allData = f sents
         aData = f aSents
         bData = f bSents

     db <- connect dbname
     createTables db (aData, bData, allData)
     disconnect db

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
