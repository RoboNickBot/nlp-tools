module NLP.Tools.Database ( connectDB
                          , disconnectDB
                          , Statement
                          , Database
                          , createDB

                          , trigramTableLF
                          , trigramTableTF
                          , lengthTable

                          , fetch
                          , fetchSt
                          , fetchAllLengths

                          , insertLengths
                          , insertTriGrams ) where

import Database.HDBC
import Database.HDBC.Sqlite3

import System.IO (hPutStrLn, stderr)
import Control.Exception (evaluate)
import Control.Applicative
import qualified Data.Map as M
import qualified Data.List as L

import NLP.General
import NLP.Freq
import NLP.Tools.Convenience

import Debug.Trace (trace)

trigramTableLF = "trigramTableLF"
trigramTableTF = "trigramTableTF"
lengthTable = "lengthTable"

data Database = DB { conn :: Connection
                   , iTrigs :: Statement
                   , iLengths :: Statement }

connectDB p = do c <- connectSqlite3 p
                 DB c <$> insertTriGramsSt c <*> insertLengthsSt c

disconnectDB = disconnect . conn

createDB p = do c <- connectSqlite3 p
                sequence_ (fmap (\t -> run c t []) tables)
                commit c
                disconnect c


fetchAllLengths :: Database -> String -> IO (M.Map String Double)
fetchAllLengths db dataset = 
  mkMap <$> quickQuery' c ("SELECT language, length FROM " 
                           ++ lengthTable
                           ++ " WHERE dataset = ?") 
                          [toSql dataset]
  where c = conn db
        mkMap vs = M.fromList (fmap prow vs)
        prow [la,le] = (fromSql la, fromSql le)

fetchSt = fetchTriGramsSt

fetchTriGramsSt :: Database -> [TriGram] -> IO Statement
fetchTriGramsSt db trigrams = 
  prepare c ("SELECT trigram, frequency FROM " ++ trigramTableLF
             ++ " WHERE dataset = ? AND language = ? AND ( "
             ++ tpreds ++ " )")
  where pred t = "trigram = alp"
        tpreds = (concat . L.intersperse " OR " . fmap pred) ts
        ts = fmap trig2str trigrams
        c = conn db
        
trig2str :: TriGram -> String
trig2str t = fmap fromTok [tri1 t, tri2 t, tri3 t]

fetch = fetchTriGrams

fetchTriGrams :: Statement 
              -> String 
              -> String
              -> [TriGram]
              -> IO (FreqList TriGram)
fetchTriGrams st set lang trigrams = 
  execute st ([toSql set, toSql lang] ++ fmap (toSql . trig2str) trigrams)
  >> (FreqList <$> mkMap M.empty)
  where mkMap :: M.Map TriGram Int -> IO (M.Map TriGram Int)
        mkMap m = fetchRow st >>= rowsert m
        rowsert m r = case r of
                        Just [tr,fr] -> mkMap (M.insert (toTrig (fromSql tr))
                                                        (fromSql fr) 
                                                        m)
                        _ -> return m
        toTrig :: String -> TriGram
        toTrig [a,b,c] = TriGram (toTok a) (toTok b) (toTok c)

insertLengthsSt :: Connection -> IO Statement
insertLengthsSt c = prepare c ("INSERT INTO " ++ lengthTable
                               ++ " VALUES (?, ?, ?)")
                               
insertLengths :: Database 
              -> [(String,String,Double)]
              -> IO ()
insertLengths db rows = 
  do let prepRow (set,la,le) = [toSql set,toSql la,toSql le]
     executeMany (iLengths db) (fmap prepRow rows)
     commit (conn db)

insertTriGramsSt :: Connection -> IO Statement
insertTriGramsSt c = prepare c ("INSERT INTO " ++ trigramTableLF
                                ++ " VALUES (?, ?, ?, ?)")
                                
insertTriGrams :: Database 
               -> [(String, String, TriGram, Int)] 
               -> IO ()
insertTriGrams db rows = 
  do let prepRow (s,l,tr,f) = [toSql s
                              ,toSql l
                              ,toSql (tt tr)
                              ,toSql f]
         tt t = fmap fromTok [tri1 t,tri2 t, tri3 t]
     executeMany (iTrigs db) (fmap prepRow rows)
     commit (conn db)

tables = ["CREATE TABLE " ++ trigramTableLF
          ++ " (dataset TEXT NOT NULL, \
             \language TEXT NOT NULL, \
             \trigram CHARACTER(3) NOT NULL, \
             \frequency INT NOT NULL)"

         ,"CREATE TABLE " ++ trigramTableTF
          ++ " (dataset TEXT NOT NULL, \
             \trigram CHARACTER(3) NOT NULL, \
             \language TEXT NOT NULL, \
             \frequency INT NOT NULL)"

         ,"CREATE TABLE " ++ lengthTable
          ++ " (dataset TEXT NOT NULL, \
             \language TEXT NOT NULL, \
             \length DOUBLE NOT NULL)"]


-- data Database = Database { conn :: Connection
--                          , getAllTriGrams :: Statement
--                          , getATriGrams :: Statement
--                          , getBTriGrams :: Statement
--                          , getLangNames :: Statement
--                          , getLen :: Statement
--                          , insertAllTriGrams :: Statement
--                          , insertATriGrams :: Statement
--                          , insertBTriGrams :: Statement }
-- 
-- insertLen db (l,n) = prepare (conn db) ("INSERT INTO " ++ nameLens
--                                         ++ " VALUES (?, ?)") >>= (\s -> execute s [toSql l, toSql n]) >> commit (conn db)
-- 
-- disconnectDB :: Database -> IO ()
-- disconnectDB = disconnect . conn
-- 
-- connectDB :: String -> IO Database
-- connectDB dbname = do putStrLn "Check"
--                       c <- connectSqlite3 dbname
--                       db <- mkStatements c
--                       evaluate db
-- 
-- createDB :: String -> IO Database
-- createDB dbname = do c <- connectSqlite3 dbname
--                      createTables c
--                      mkStatements c
-- 
-- mkStatements :: Connection -> IO Database
-- mkStatements c = 
--   do sAllTriGrams <- insertSt c nameTriGrams 
--      sATriGrams <- insertSt c nameAData
--      sBTriGrams <- insertSt c nameBData
--      gAllTriGrams <- fetchSt c nameTriGrams
--      gATriGrams <- fetchSt c nameAData
--      gBTriGrams <- fetchSt c nameBData
--      gLangNames <- fetchLangsSt c
--      gGetLen <- fetchLangLen c
--      evaluate (Database c
--                         gAllTriGrams
--                         gATriGrams
--                         gBTriGrams
--                         gLangNames
--                         gGetLen
--                         sAllTriGrams
--                         sATriGrams
--                         sBTriGrams)
-- 
-- 
-- insertSt :: Connection -> String -> IO Statement
-- insertSt db table = prepare db ("INSERT INTO " ++ table 
--                                 ++ " VALUES (?, ?, ?, ?, ?)")
-- 
-- prepSt c s = prepare c s
-- 
-- strSt table = ("SELECT freq FROM " 
--                    ++ table
--                    ++ " WHERE lang = ? AND gram1 = ? AND gram2 = ? AND gram3 = ?")
-- 
-- fetchSt c table = 
--   prepSt c (strSt table)
-- 
-- fetchLangsSt db = prepare db ("SELECT DISTINCT lang FROM " 
--                               ++ nameTriGrams)
-- 
-- fetchLangLen db = prepare db ("SELECT len FROM " ++ nameLens ++ " WHERE names = ?")
-- 
-- createNameTable db names = 
--   do run db ("CREATE TABLE " ++ nameName
--              ++ " (names TEXT NOT NULL)") []
--      commit db
--      st <- prepare db ("INSERT INTO " ++ nameName ++ " VALUES (?)")
--      executeMany st ((fmap (: []) . fmap toSql) names)
-- 
-- 
-- createLenTable :: Connection -> IO ()
-- createLenTable db =
--   do run db ("CREATE TABLE " ++ nameLens
--              ++ " (names TEXT NOT NULL, len INT NOT NULL)") []
--      commit db
--      -- st <- prepare db ("INSERT INTO " ++ nameLens ++ " VALUES (?, ?)")
--      -- executeMany st (fmap (\(n,v) -> [toSql n, toSql v]) vals)
--      -- commit db
-- 
-- createDataTable db n = run db (trigramTable n) []
-- 
-- trigramTable n = "CREATE TABLE " 
--                  ++ n 
--                  ++ " (lang TEXT NOT NULL, \
--                     \gram1 CHARACTER(1) NOT NULL, \
--                     \gram2 CHARACTER(1) NOT NULL, \
--                     \gram3 CHARACTER(1) NOT NULL, \
--                     \freq INT NOT NULL)"
-- 
-- type QTrigs = [(String, FreqList TriGram)]
-- 
-- createTables :: Connection -> IO ()
-- createTables db = 
--   createLenTable db
--   >> createDataTable db nameAData
--   >> createDataTable db nameBData
--   >> createDataTable db nameTriGrams
--   >> commit db
-- 
-- insertLangAll db trigs = insertLang (conn db) (insertAllTriGrams db) trigs
-- insertLangA db trigs = insertLang (conn db) (insertATriGrams db) trigs
-- insertLangB db trigs = insertLang (conn db) (insertBTriGrams db) trigs
-- 
-- insertLang :: Connection -> Statement -> (String, FreqList TriGram) -> IO ()
-- insertLang c st trigs = fillTable st [trigs] >> commit c
-- 
-- fillTable :: Statement -> QTrigs -> IO ()
-- fillTable st ts =
--   do let rows :: [(String, Char, Char, Char, Int)]
--          rows = (concat . fmap flat . fmap (smap toL)) ts
--          sqlRows = fmap (\(n,g1,g2,g3,v) -> toSql n 
--                                             : toSql g1
--                                             : toSql g2
--                                             : toSql g3
--                                             : toSql v : []) 
--                         rows
--      executeMany st sqlRows
--   where flat (s,ts) = fmap (\(g1,g2,g3,c) -> (s,g1,g2,g3,c)) ts
--         toL = fmap (\(t,v) -> ( fromTok (tri1 t)
--                               , fromTok (tri2 t)
--                               , fromTok (tri3 t)
--                               , v                ))
--               . NLP.Tools.Database.toList
-- 
-- toList :: FreqList TriGram -> [(TriGram,Int)]
-- toList = M.toList . freqMap
-- 
-- fetchLangNames :: Database -> IO [String]
-- fetchLangNames db = 
--   fmap fromSql . concat 
--   <$> (execute (getLangNames db) []
--        >> fetchAllRows (getLangNames db))
-- 
-- readOut :: [[SqlValue]] -> Int
-- readOut (vs:[]) = readOne vs
-- readOut [] = 0
-- 
-- convTok = toTok . fromSql
-- 
-- convInt :: SqlValue -> Int
-- convInt = fromSql
-- 
-- readOne :: [SqlValue] -> Int
-- readOne (v:[]) = fromSql v
-- readOne _ = error "readOne failed : empty list?"
-- 
-- --readOne :: [SqlValue] -> (TriGram, Int)
-- --readOne (g1:g2:g3:c:[]) = let f = convTok
-- --                            in (TriGram (f g1) (f g2) (f g3), convInt c)
-- 
-- toFreqL :: [(TriGram, Int)] -> FreqList TriGram
-- toFreqL = FreqList . (M.fromList :: [(TriGram,Int)] -> M.Map TriGram Int)
-- 
-- sortOut :: [(String, TriGram, Int)] -> String -> (String, FreqList TriGram)
-- sortOut gs lang = (lang, FreqList
--                          . M.fromList 
--                          . fmap (\(a,b,c) -> (b,c)) 
--                          . filter (\(a,b,c) -> a == lang) $ gs)
-- 
-- 
-- fetchTriGrams' :: Statement -> String -> TriGram -> IO Int
-- fetchTriGrams' st lang tr = readOut 
--                             <$> (execute st [toSql lang
--                                             ,toSql (fromTok.tri1$tr)
--                                             ,toSql (fromTok.tri2$tr)
--                                             ,toSql (fromTok.tri3$tr)] >> fetchAllRows' st)
-- 
-- fetchTriGrams db lang tr = fetchTriGrams' (getAllTriGrams db) lang tr
-- fetchAData db lang tr = fetchTriGrams' (getATriGrams db) lang tr
-- fetchBData db lang tr = fetchTriGrams' (getBTriGrams db) lang tr
-- 
-- fetchLen db lang = readOut <$> (execute (getLen db) [toSql lang] >> fetchAllRows' (getLen db))
