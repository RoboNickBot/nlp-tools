module NLP.Tools.Database ( connect 
                          , disconnect
                          , Statement
                          , Database
                          
                          , createTables
                          , fetchLangNames
                          , fetchTriGrams
                          , fetchAData
                          , fetchBData     ) where
                          
import Database.HDBC
import Database.HDBC.Sqlite3

import System.IO (hPutStrLn, stderr)
import Control.Applicative

import NLP.General
import NLP.Freq
import NLP.Tools.Convenience

import Debug.Trace (trace)

nameAData = "adata"
nameBData = "bdata"
nameTriGrams = "trigrams"
nameName = "names"

type Database = Connection

connect = connectSqlite3

createNameTable db names = 
  do run db ("CREATE TABLE " ++ nameName
             ++ " (names TEXT NOT NULL)") []
     commit db
     st <- prepare db ("INSERT INTO " ++ nameName ++ " VALUES (?)")
     executeMany st ((fmap (: []) . fmap toSql) names)

createDataTable db n = run db (duoTable n) []

duoTable n = "CREATE TABLE " 
             ++ n 
             ++ " (lid TEXT NOT NULL, \
                \ldata TEXT NOT NULL)"

type QTrigs = [(String, FreqList TriGram)]

createTables :: Database -> (QTrigs,QTrigs,QTrigs) -> IO ()
createTables db (a,b,t) = 
  createNameTable db (fmap fst t)
  >> createDataTable db nameAData
  >> createDataTable db nameBData
  >> createDataTable db nameTriGrams
  >> commit db
  >> fillTables db (a,b,t)
  >> commit db

insertSt :: Database -> String -> IO Statement
insertSt c n = prepare c ("INSERT INTO " 
                          ++ n ++ " VALUES (?, ?)")

duosert :: Statement -> String -> (String, String) -> IO ()
duosert st n (l,d) = execute st [toSql l, toSql d]
                     >> hPutStrLn stderr txt
  where txt = ("Table " ++ n ++ " :: Inserted " ++ l ++ " ...")

supersert :: Database -> String -> [(String,String)] -> IO ()
supersert db name vs = sequence_ (fmap (dsrt db name) vs)
  where dsrt db n v = do st <- insertSt db n
                         duosert st n v

fillTables :: Database -> (QTrigs,QTrigs,QTrigs) -> IO ()
fillTables db (a,b,t) = 
  let f = (fmap (smap (show :: FreqList TriGram -> String)))
      (as,bs,ts) = (f a, f b, f t)
  in do supersert db nameAData as
        supersert db nameBData bs
        supersert db nameTriGrams ts 

fetchLangNames :: Database -> IO [String]
fetchLangNames db = 
  fmap fromSql . concat 
  <$> quickQuery' db ("SELECT * from " ++ nameName) []

getLangValue :: Database -> String -> String -> IO ([[SqlValue]])
getLangValue db lang table = 
  quickQuery db ("SELECT lid, ldata from " 
                 ++ table
                 ++ " where lid = ?") [toSql lang]

readOut vs = head (fmap readOne vs)

readOne (a:b:[]) = (fromSql a, fromSql b)

type GetProfile = Database -> String -> IO (String, FreqList TriGram)

fetchTriGrams :: GetProfile 
fetchTriGrams db lang = 
  smap read 
  <$> readOut 
  <$> getLangValue db lang nameTriGrams

fetchAData :: GetProfile
fetchAData db lang = 
  smap read
  <$> readOut
  <$> getLangValue db lang nameAData

fetchBData :: GetProfile
fetchBData db lang = 
  smap read
  <$> readOut
  <$> getLangValue db lang nameBData

