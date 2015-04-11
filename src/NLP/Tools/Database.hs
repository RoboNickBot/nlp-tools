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
import qualified Data.Map as M

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

createDataTable db n = run db (trigramTable n) []

trigramTable n = "CREATE TABLE " 
                 ++ n 
                 ++ " (lang TEXT NOT NULL, \
                    \trigram TEXT NOT NULL,\
                    \freq INT NOT NULL)"

type QTrigs = [(String, FreqList TriGram)]

createTables :: Database -> (QTrigs,QTrigs,QTrigs) -> IO ()
createTables db (a,b,t) = 
  createNameTable db (fmap fst t)
  >> createDataTable db nameAData
  >> createDataTable db nameBData
  >> createDataTable db nameTriGrams
  >> commit db
  >> fillTable db nameAData a
  >> fillTable db nameBData b
  >> fillTable db nameTriGrams t
  >> commit db

fillTable :: Database -> String -> QTrigs -> IO ()
fillTable db n ts =
  do st <- prepare db ("INSERT INTO " ++ n ++ " VALUES (?, ?, ?)")
     let rows :: [(String, String, Int)]
         rows = (concat . fmap flat . fmap (smap toL)) ts
         sqlRows = fmap (\(n,t,v) -> toSql n : toSql t : toSql v : []) 
                        rows
     executeMany st sqlRows
  where flat (s,ts) = fmap (\(b,c) -> (s,b,c)) ts
        toL = fmap (\(t,v) -> (show t, v))
              . NLP.Tools.Database.toList

toList :: FreqList TriGram -> [(TriGram,Int)]
toList = M.toList . freqMap

fetchLangNames :: Database -> IO [String]
fetchLangNames db = 
  fmap fromSql . concat 
  <$> quickQuery db ("SELECT * from " ++ nameName) []

getLangValues :: Database -> String -> String -> IO ([[SqlValue]])
getLangValues db lang table = 
  quickQuery db ("SELECT trigram, freq FROM " 
                 ++ table
                 ++ " WHERE lang = ?") [toSql lang]

readOut vs = (fmap readOne vs)

readOne :: [SqlValue] -> (String, Int)
readOne (b:c:[]) = (fromSql b, fromSql c)

toFreqL :: [(String, Int)] -> FreqList TriGram
toFreqL = FreqList . M.fromList . fmap (\(s,i) -> (read s, i))

fetchTriGrams' :: Database -> String -> String 
               -> IO (String, FreqList TriGram)
fetchTriGrams' db name lang = 
  (,) lang
  <$> toFreqL
  <$> readOut 
  <$> getLangValues db lang name

fetchTriGrams db lang = fetchTriGrams' db nameTriGrams lang
fetchAData db lang = fetchTriGrams' db nameAData lang
fetchBData db lang = fetchTriGrams' db nameBData lang
