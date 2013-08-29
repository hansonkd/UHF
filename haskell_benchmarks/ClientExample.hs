{-# LANGUAGE OverloadedStrings      #-}

module Main where



import           Data.Maybe (fromMaybe)
import           Control.Monad (void, forM_)
import           Control.Monad.Trans (liftIO, MonadIO)

import qualified Data.Bson as B

import           Data.Binary.Get (runGet)
import           Data.Binary.Put (runPut)
import           Data.Bson.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import           Data.Conduit
import           Data.Conduit.Network

import           Codec.Compression.Zlib

import           UFdb.BenchmarksCommon

main :: IO ()
main = do
    runTCPClient (clientSettings 5002 "127.0.0.1") clientApp

clientApp :: (MonadIO m, MonadUnsafeIO m, MonadThrow m) => Application m
clientApp appData = let src = appSource appData
                        sink = appSink appData
                    in src $= benchmarkConduit $$ sink
                    --(decompress defaultWindowBits) $=


compressor = compressWith defaultCompressParams { compressLevel = bestSpeed }

benchmarkConduit :: MonadIO m => Conduit B.ByteString m (B.ByteString)
benchmarkConduit = do

    liftIO $ putStrLn "Putting 1000 small documents to server individually..."
    timeIt $ forM_ testRangeInsert (\x -> do
        yield $ BL.toStrict $ runPut $ putDocument $ serializedPutSmall x)

    liftIO $ putStrLn "Done. \nPutting 1000 big documents to server individually..."
    timeIt $ forM_ testRangeInsert (\x -> do
        yield $ BL.toStrict $ runPut $ putDocument $ serializedPutBig x)
    
    liftIO $ putStrLn "Done Putting \nGetting documents from server 100 times..."
    timeIt $ forM_ testRangeSearch (\x -> do
        yield $ BL.toStrict $ runPut $ putDocument $ serializedGetUnion x
        void $ await)
    liftIO $ putStrLn "Done!"