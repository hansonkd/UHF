{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import           Database.MongoDB
import           Control.Monad.Trans (liftIO, MonadIO)
import           Control.Monad

import           Control.Monad (forM_)
import qualified Data.Bson as B

import           Data.Binary.Put (runPut)
import           Data.Bson.Binary

import           UFdb.BenchmarksCommon

main = do
    pipe <- runIOE $ connect (host "127.0.0.1")
    putStrLn "Now Clearing"
    void $ timeIt $ access pipe master "test" $ clearDocuments
    putStrLn "Done. \nPutting 1000 small documents to server individually..."
    void $ timeIt $ access pipe master "test" $ forM_ testRangeInsert (\x -> insertSmallDocuments x)
    putStrLn "Done. \nPutting 1000 big documents to server individually..."
    void $ timeIt $ access pipe master "test" $ forM_ testRangeInsert (\x -> insertBigDocuments x)
    putStrLn "Done. \nNow Searching over documents 100x..."
    void $ timeIt $ access pipe UnconfirmedWrites "test" $ forM_ testRangeSearch (\x -> searchDocuments x)
    putStrLn "Showing results of query:"
    result <- access pipe master "test" $ do
                    docs <- searchDocuments 1
                    liftIO $ print $ length docs
    putStrLn "Done."
    close pipe
    
-- | mapFn = Javascript [] "function() {this.test20.forEach (function(z) {emit(1, z);});}"
-- | reduceFn = Javascript [] "function (key, values) {var total = 0; for (var i = 0; i < values.length; i++) {total += values[i];} return total;}"
-- | forM_ [0..100] $ (\_ -> runMR' (mapReduce "db_test" mapFn reduceFn))

serializedSelect x = [ "test20.test21" B.:= (B.Doc $ [ "$lt" B.:= B.Float (x * 20)]), "test21" B.:= (B.Doc $ [ "$gt" B.:= B.Float x]) ]


clearDocuments = delete (select [] "db_test")

insertSmallDocuments x = insert "db_test" $ serializedPutSmall x

insertBigDocuments x = insert "db_test" $ serializedPutBig x

searchDocuments x = nextN 10 =<< find (select (serializedSelect x) "db_test") {sort = []}
