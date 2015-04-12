module NLP.Tools.Database ( connect 
                          , disconnect
                          , Statement
                          , Database
                          
                          , nameAData
                          , nameBData
                          , nameTriGrams
                          
                          , createTables
                          , insertLang
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
                    \gram1 CHARACTER(1) NOT NULL, \
                    \gram2 CHARACTER(1) NOT NULL, \
                    \gram3 CHARACTER(1) NOT NULL, \
                    \freq INT NOT NULL)"

type QTrigs = [(String, FreqList TriGram)]

createTables :: Database -> IO ()
createTables db = 
  createDataTable db nameAData
  >> createDataTable db nameBData
  >> createDataTable db nameTriGrams
  >> commit db

insertLang :: Database -> String -> (String, FreqList TriGram) -> IO ()
insertLang db n trigs = fillTable db n [trigs] >> commit db

fillTable :: Database -> String -> QTrigs -> IO ()
fillTable db n ts =
  do st <- prepare db ("INSERT INTO " ++ n 
                       ++ " VALUES (?, ?, ?, ?, ?)")
     let rows :: [(String, Char, Char, Char, Int)]
         rows = (concat . fmap flat . fmap (smap toL)) ts
         sqlRows = fmap (\(n,g1,g2,g3,v) -> toSql n 
                                            : toSql g1
                                            : toSql g2
                                            : toSql g3
                                            : toSql v : []) 
                        rows
     executeMany st sqlRows
  where flat (s,ts) = fmap (\(g1,g2,g3,c) -> (s,g1,g2,g3,c)) ts
        toL = fmap (\(t,v) -> ( fromTok (tri1 t)
                              , fromTok (tri2 t)
                              , fromTok (tri3 t)
                              , v                ))
              . NLP.Tools.Database.toList

toList :: FreqList TriGram -> [(TriGram,Int)]
toList = M.toList . freqMap

fetchLangNames :: Database -> IO [String]
fetchLangNames db = 
  fmap fromSql . concat 
  <$> quickQuery' db ("SELECT DISTINCT lang FROM " 
                      ++ nameTriGrams) []

getLangValues :: Database -> String -> String -> IO ([[SqlValue]])
getLangValues db lang table = 
  quickQuery' db ("SELECT gram1, gram2, gram3, freq FROM " 
                  ++ table
                  ++ " WHERE lang = ?") [toSql lang]

readOut :: [[SqlValue]] -> [(TriGram, Int)]
readOut vs = (fmap readOne vs)

readOne :: [SqlValue] -> (TriGram, Int)
readOne (g1:g2:g3:c:[]) = let f = toTok . fromSql
                          in (TriGram (f g1) (f g2) (f g3), fromSql c)

toFreqL :: [(TriGram, Int)] -> FreqList TriGram
toFreqL = FreqList . (M.fromList :: [(TriGram,Int)] -> M.Map TriGram Int)

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
