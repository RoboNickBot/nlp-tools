import Database.HDBC 
import Database.HDBC.Sqlite3
import Filesystem
import Filesystem.Path.CurrentOS
import Options.Applicative
import Data.Monoid

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

testdataN = "testdata"
maindataN = "maindata"

mkdatabase (Opts dbname dataroot prc) = 
  do putStrLn "eh"
     removeFile (decodeString dbname)
     db <- conn dbname
     run db (trifreqTable testdataN) []
     run db (trifreqTable maindataN) []
     return ()


conn = connectSqlite3

ctest cn = run cn "CREATE TABLE test (id INTEGER NOT NULL, desc VARCHAR(80))" []

cinsr cn = run cn "INSERT INTO test (id) VALUES (0)" []

trifreqTable n = "CREATE TABLE " ++ n ++ " (lang TEXT NOT NULL, trigrams TEXT NOT NULL)"

