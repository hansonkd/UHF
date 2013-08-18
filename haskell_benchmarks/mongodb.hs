{-# LANGUAGE OverloadedStrings, DeriveGeneric,  TypeOperators #-}

import qualified Data.Text as T
import           Database.MongoDB
import           Control.Monad.Trans (liftIO, MonadIO)
import           Control.Monad

import           Control.Monad (forM_)
import qualified Data.Bson as B
import           Data.Bson.Generic

import           GHC.Generics

import           Data.Binary.Put (runPut)
import           Data.Bson.Binary


data TestBsonData = TestBsonData { test20 :: [Int], test21 :: String }
  deriving (Generic, Show, Eq)
instance ToBSON TestBsonData
instance FromBSON TestBsonData

sampleData :: B.Document
sampleData = toBSON $ TestBsonData [4] "bb"

serialized x = toBSON $ TestBsonData [x] "bb"

main = do
    pipe <- runIOE $ connect (host "127.0.0.1")
    e <- access pipe master "test" run
    print e
    close pipe
    
mapFn = Javascript [] "function() {this.test20.forEach (function(z) {emit(1, z);});}"
reduceFn = Javascript [] "function (key, values) {var total = 0; for (var i = 0; i < values.length; i++) {total += values[i];} return total;}"

run = do
    clearDcouments
    liftIO $ putStrLn "Done clearing..."
    insertDocuments
    liftIO $ putStrLn "Done inserting..."
    allTeams >>= (\_ -> liftIO $ putStrLn "Done fetching..")
    forM_ [0..100] $ (\_ -> runMR' (mapReduce "db_test" mapFn reduceFn))




clearDcouments = delete (select [] "db_test")

insertDocuments = forM_ [0..1000] (\x -> insert "db_test" $ serialized x)

allTeams = rest =<< find (select [] "db_test") {sort = []}
