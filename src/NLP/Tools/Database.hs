{-# LANGUAGE BangPatterns #-}
module NLP.Tools.Database ( createDB
                          , connectDB
                          , disconnectDB
                          , Statement
                          , Database
                          
                          , nameAData
                          , nameBData
                          , nameTriGrams
                          
                          , insertLangAll
                          , insertLangA
                          , insertLangB
                          , fetchLangNames
                          , fetchTriGrams
                          , fetchAData
                          , fetchBData     ) where
                          
import Database.HDBC
import Database.HDBC.Sqlite3

import System.IO (hPutStrLn, stderr)
import Control.Exception (evaluate)
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

data Database = Database { conn :: !Connection
                         , getAllTriGrams :: !Statement
                         , getATriGrams :: !Statement
                         , getBTriGrams :: !Statement
                         , getLangNames :: !Statement
                         , insertAllTriGrams :: !Statement
                         , insertATriGrams :: !Statement
                         , insertBTriGrams :: !Statement }

disconnectDB :: Database -> IO ()
disconnectDB = disconnect . conn

connectDB :: String -> IO Database
connectDB dbname = do putStrLn "Check"
                      c <- connectSqlite3 dbname
                      db <- mkStatements c
                      evaluate db

createDB :: String -> IO Database
createDB dbname = do c <- connectSqlite3 dbname
                     createTables c
                     mkStatements c

mkStatements :: Connection -> IO Database
mkStatements c = 
  do sAllTriGrams <- insertSt c nameTriGrams
     sATriGrams <- insertSt c nameAData
     sBTriGrams <- insertSt c nameBData
     gAllTriGrams <- fetchSt c nameTriGrams
     gATriGrams <- fetchSt c nameAData
     gBTriGrams <- fetchSt c nameBData
     gLangNames <- fetchLangsSt c
     evaluate (Database c
                        gAllTriGrams
                        gATriGrams
                        gBTriGrams
                        gLangNames
                        sAllTriGrams
                        sATriGrams
                        sBTriGrams)


insertSt :: Connection -> String -> IO Statement
insertSt db table = prepare db ("INSERT INTO " ++ table 
                                ++ " VALUES (?, ?, ?, ?, ?)")

prepSt c s = prepare c s

strSt table = ("SELECT gram1, gram2, gram3, freq FROM " 
                                        ++ table
                                        ++ " WHERE lang = ?")

fetchSt !c !table = 
  prepSt c (strSt table)

fetchLangsSt db = prepare db ("SELECT DISTINCT lang FROM " 
                              ++ nameTriGrams)

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

createTables :: Connection -> IO ()
createTables db = 
  createDataTable db nameAData
  >> createDataTable db nameBData
  >> createDataTable db nameTriGrams
  >> commit db

insertLangAll db trigs = insertLang (conn db) (insertAllTriGrams db) trigs
insertLangA db trigs = insertLang (conn db) (insertATriGrams db) trigs
insertLangB db trigs = insertLang (conn db) (insertBTriGrams db) trigs

insertLang :: Connection -> Statement -> (String, FreqList TriGram) -> IO ()
insertLang c st trigs = fillTable st [trigs] >> commit c

fillTable :: Statement -> QTrigs -> IO ()
fillTable st ts =
  do let rows :: [(String, Char, Char, Char, Int)]
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
  <$> (execute (getLangNames db) []
       >> fetchAllRows (getLangNames db))

readOut :: [[SqlValue]] -> [(TriGram, Int)]
readOut vs = (fmap readOne vs)

convTok = toTok . fromSql

convInt :: SqlValue -> Int
convInt = fromSql

readOne :: [SqlValue] -> (TriGram, Int)
readOne (g1:g2:g3:c:[]) = let f = convTok
                          in (TriGram (f g1) (f g2) (f g3), convInt c)

toFreqL :: [(TriGram, Int)] -> FreqList TriGram
toFreqL = FreqList . (M.fromList :: [(TriGram,Int)] -> M.Map TriGram Int)

sortOut :: [(String, TriGram, Int)] -> String -> (String, FreqList TriGram)
sortOut gs lang = (lang, FreqList
                         . M.fromList 
                         . fmap (\(a,b,c) -> (b,c)) 
                         . filter (\(a,b,c) -> a == lang) $ gs)


fetchTriGrams' :: Statement -> [String] -> IO [(String, FreqList TriGram)]
fetchTriGrams' st langs = (\gs -> fmap (sortOut gs) langs) 
                          <$> readOut 
                          <$> (execute st [] >> fetchAllRows st)

fetchTriGrams db lang = fetchTriGrams' (getAllTriGrams db) lang
fetchAData db lang = fetchTriGrams' (getATriGrams db) lang
fetchBData db lang = fetchTriGrams' (getBTriGrams db) lang
