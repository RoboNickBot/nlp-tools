import System.Directory
import System.IO (hPutStrLn, stderr)
import System.IO.Strict (readFile)
import Options.Applicative
import qualified Data.List as L

import NLP.General
import NLP.Freq
import NLP.Tools

percent :: Parser Int
percent = option auto
            ( long "percent" 
              <> short 'p'
              <> metavar "PERCENT"
              <> value 10
              <> showDefault
              <> help "Percentage of text to be withheld for \
                      \testing, as a number in the \
                      \range [0,100]" )

dbname :: Parser String
dbname = strOption 
           ( long "output" 
             <> short 'o'
             <> metavar "FILENAME"
             <> value "nlp.db"
             <> showDefault
             <> help "Filename of the database to be built" )

rootdir :: Parser String
rootdir = strOption
            ( long "source"
              <> short 's'
              <> metavar "DIRECTORY"
              <> value "/data/crubadan"
              <> showDefault
              <> help "Root directory of source language data" )

data Opts = Opts String String Int

parser = Opts <$> dbname <*> rootdir <*> percent

opts = info (helper <*> parser) 
            ( fullDesc
              <> progDesc "Build database for NLP Tools from \
                          \language data on the filesystem"
              <> header "builddb - build nlp database")

main = execParser opts >>= mkdatabase

mkdatabase (Opts dbname dataroot prc) = 
  do db <- connect dbname
     createTable db testdataN
     createTable db maindataN 
     s1 <- insertSt db testdataN
     s2 <- insertSt db maindataN 

     files <- datafilenames dataroot
     sequence_ (processLang' db prc (s1,s2) <$> files)
     disconnect db

processLang' c i ss d = 
  processLang i ss d 
  >> commit c
  >> hPutStrLn stderr ((fst d) ++ " stored...")

datafilenames root = 
  fmap (\n -> (n, root ++ "/" ++ n ++ "/SAMPSENTS"))
       <$> (datadirs root)

datadirs = fmap (L.delete ".") 
           . fmap (L.delete "..") 
           . getDirectoryContents

datafile (lang,fn) = 
  (,) lang <$> System.IO.Strict.readFile fn

smap :: (b -> c) -> (a, b) -> (a, c)
smap f (a,b) = (a,f b)

ftrig :: String -> FreqList TriGram
ftrig = features

processLang :: Int 
            -> (Statement,Statement) 
            -> (String, String) 
            -> IO ()
processLang p sts fn = 
  do ds <- splitLang p <$> datafile fn
     duosert (fst sts) ((smap (show . ftrig) . fst) ds)
     duosert (snd sts) ((smap (show . ftrig) . snd) ds)

splitLang :: Int 
          -> (String, String) 
          -> ((String, String), (String, String))
splitLang p (l,d) = let dls = lines d
                        i = (length dls) * 100 `div` p
                        (d1,d2) = splitAt i dls
                    in ( (l, concat d1)
                       , (l, concat d2))
