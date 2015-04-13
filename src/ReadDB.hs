import Options.Applicative

import NLP.Tools

langid :: Parser String
langid = strOption
           ( long "lang" 
             <> short 'l'
             <> metavar "CODE"
             <> help "Language to read" )

dbname :: Parser String
dbname = strOption 
           ( long "database" 
             <> short 'd'
             <> metavar "FILENAME"
             <> value "nlp.db"
             <> showDefault
             <> help "Filename of the database to read" )

data Opts = Opts String String

parser = Opts <$> dbname <*> langid

opts = info (helper <*> parser)
            ( fullDesc 
              <> progDesc "Dump data on a particular \
                                   \language from the database"
              <> header "readdb - poke at the database")

main = execParser opts >>= readdatabase

readdatabase (Opts dbname langid) =
  do db <- connectDB dbname
     a <- fetchAData db langid
     b <- fetchBData db langid
     putStrLn ((show . snd $ a) 
               ++ "\n\nBREAK\n\n" 
               ++ (show . snd $ b))
