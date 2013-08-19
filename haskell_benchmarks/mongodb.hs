{-# LANGUAGE OverloadedStrings, DeriveGeneric,  TypeOperators, DeriveDataTypeable #-}

import qualified Data.Text as T
import           Database.MongoDB
import           Control.Monad.Trans (liftIO, MonadIO)
import           Control.Monad
import           Control.Monad.Trans.Control

import           Control.Monad (forM_)
import qualified Data.Bson as B
import           Data.Bson.Generic

import           GHC.Generics
import           Data.Typeable

import           Data.Binary.Put (runPut)
import           Data.Bson.Binary

import           System.TimeIt

data TestBsonData = TestBsonData { test20 :: Maybe TestBsonData, test21 :: Double, test22 :: [Double] }
  deriving (Generic, Show, Typeable, Eq)
instance ToBSON TestBsonData
instance FromBSON TestBsonData

serializedPutSmall x = toBSON $ TestBsonData (Just $ TestBsonData Nothing (x*10) []) x []

serializedPutBig x = toBSON $ TestBsonData (Just $ TestBsonData Nothing (x*10) [1..500]) x [1..500]


testRangeInsert = [0..100]
testRangeSearch = [0..10]

main = do
    pipe <- runIOE $ connect (host "127.0.0.1")
    putStrLn "Now Clearing"
    void $ timeIt $ access pipe master "test" $ clearDocuments
    putStrLn "Done. \nPuting 1000 small documents to server individually..."
    void $ timeIt $ access pipe master "test" $ forM_ testRangeInsert (\x -> insertSmallDocuments x)
    putStrLn "Done. \nPuting 1000 big documents to server individually..."
    void $ timeIt $ access pipe master "test" $ forM_ testRangeInsert (\x -> insertBigDocuments x)
    putStrLn "Done. \nNow Searching over documents 100x..."
    void $ timeIt $ access pipe UnconfirmedWrites "test" $ forM_ testRangeSearch (\_ -> void searchDocuments)
    putStrLn "Showing results of query:"
    result <- access pipe master "test" $ do
                    docs <- searchDocuments
                    liftIO $ print $ length docs
    putStrLn "Done."
    close pipe
    
-- | mapFn = Javascript [] "function() {this.test20.forEach (function(z) {emit(1, z);});}"
-- | reduceFn = Javascript [] "function (key, values) {var total = 0; for (var i = 0; i < values.length; i++) {total += values[i];} return total;}"
-- | forM_ [0..100] $ (\_ -> runMR' (mapReduce "db_test" mapFn reduceFn))

serializedSelect = [ "test20.test21" B.:= (B.Doc $ [ "$lt" B.:= B.Int32 100]), "test21" B.:= (B.Doc $ [ "$gt" B.:= B.Int32 5]) ]

clearDocuments = delete (select [] "db_test")

insertSmallDocuments x = insert "db_test" $ serializedPutSmall x

insertBigDocuments x = insert "db_test" $ serializedPutBig x

searchDocuments = nextN 10 =<< find (select serializedSelect "db_test") {sort = []}
