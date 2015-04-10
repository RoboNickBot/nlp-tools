import Options.Applicative

import NLP.Tools

langid :: Parser String
langid = strOption
           ( long "lang" 
             <> short 'l'
             <> metavar "CODE" )
             
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
  do c <- connect dbname
     d <- fetchLangData c langid
     putStrLn (((snd . fst) d) 
               ++ "\n\nBREAK\n\n" 
               ++ ((snd . snd) d))
