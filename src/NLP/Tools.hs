module NLP.Tools ( choosebest
                 , smap

                 , connect
                 , disconnect
                 , Statement
                 , IConnection
                 , commit
                 
                 , duosert
                 , insertSt
                 , testdataN
                 , maindataN
                 , createTable
                 , createNameTable
                 , getLangNames
                 , fetchLangData
                 , fetchLangMainData
                 , fetchLangTestData ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Applicative

import NLP.General
import NLP.Freq

choosebest :: [(String, FreqList TriGram)] -> FreqList TriGram -> String
choosebest fls fl = (bestof . fmap (evaluate fl)) fls

bestof :: Ord b => [(a,b)] -> a
bestof (f:rs) = fst . foldr (\(a,b) (c,d) -> if b > d
                                                then (a,b)
                                                else (c,d))
                            f $ rs
                            
evaluate :: (FreqList TriGram) 
         -> (String, FreqList TriGram) 
         -> (String, Double)
evaluate f (n,p) = (n, cosine f p)

testdataN = "testdata"
maindataN = "maindata"
namedataN = "namedata"

insertSt :: IConnection c => c -> String -> IO Statement
insertSt c n = prepare c ("INSERT INTO " 
                          ++ n ++ " VALUES (?, ?)")
                          
duosert :: Statement -> (String, String) -> IO ()
duosert st (l,d) = execute st [toSql l, toSql d] 
                       >> return ()

connect = connectSqlite3

getLangNames :: IConnection c => c -> IO [String]
getLangNames db = 
  fmap fromSql . concat 
  <$> quickQuery' db ("SELECT * from " ++ namedataN) []

createNameTable db names = 
  do run db ("CREATE TABLE " ++ namedataN 
             ++ " (names TEXT NOT NULL)") []
     commit db
     st <- prepare db ("INSERT INTO " ++ namedataN ++ " VALUES (?)")
     executeMany st ((fmap (: []) . fmap toSql) names)
     commit db

createTable db n = run db (duoTable n) [] >> commit db

duoTable n = "CREATE TABLE " 
             ++ n 
             ++ " (lid TEXT NOT NULL, \
                \ldata TEXT NOT NULL)"

fetchLangData :: IConnection c => c -> String -> IO ((String,String), (String,String))
fetchLangData db lang = 
  (,) <$> (readOut <$> getLangValue db lang testdataN)
      <*> (readOut <$> getLangValue db lang maindataN)

fetchLangMainData :: IConnection c => c -> String -> IO (String, String)
fetchLangMainData db lang = readOut <$> getLangValue db lang maindataN

fetchLangTestData :: IConnection c => c -> String -> IO (String, String)
fetchLangTestData db lang = readOut <$> getLangValue db lang testdataN

readOut vs = head (fmap readOne vs)

readOne (a:b:[]) = (fromSql a, fromSql b)

getLangValue :: IConnection c => c -> String -> String -> IO ([[SqlValue]])
getLangValue db lang table = 
  quickQuery db ("SELECT lid, ldata from " 
                 ++ table
                 ++ " where lid = ?") [toSql lang]

smap :: (b -> c) -> (a, b) -> (a, c)
smap f (a,b) = (a,f b)
