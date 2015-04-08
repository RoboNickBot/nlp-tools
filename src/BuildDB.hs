import Database.HDBC 
import Database.HDBC.Sqlite3
import Filesystem
import Filesystem.Path.CurrentOS
import Options.Applicative
import Control.Applicative
import Data.Monoid

percent :: Parser String
percent = strOption 
            ( long "percent" 
              <> short 'p'
              <> metavar "NUMBER"
              <> help "Percentage of text to be withheld for testing")

dbname :: Parser String
dbname = strOption 
           ( long "database" 
             <> short 'd'
             <> metavar "FILENAME"
             <> help "Filename of database to be built" )

data Opts = Opts String String

parser = Opts <$> dbname <*> percent

main = execParser (info parser mempty) >>= mkdatabase

testdataN = "testdata"
maindataN = "maindata"

mkdatabase (Opts name prc) = 
  do putStrLn "eh"
     removeFile (decodeString name)
     db <- conn name
     run db (trifreqTable testdataN) []
     run db (trifreqTable maindataN) []
     return ()


conn = connectSqlite3

ctest cn = run cn "CREATE TABLE test (id INTEGER NOT NULL, desc VARCHAR(80))" []

cinsr cn = run cn "INSERT INTO test (id) VALUES (0)" []

trifreqTable n = "CREATE TABLE " ++ n ++ " (lang TEXT NOT NULL, trigrams TEXT NOT NULL)"

