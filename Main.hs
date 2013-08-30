{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric          #-}

module Main where

import           UFdb.Types
import           UFdb.Actions

import           Control.Concurrent (forkIO)
import           Control.Monad.Trans (liftIO, MonadIO, lift)
import           Data.Acid
import           Data.Maybe (fromMaybe)
import qualified Data.Bson as B
import           Data.Bson.Generic
import           Data.Bson.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import           Data.ByteString.Char8 as BS
import           Data.ByteString.Lazy  as BSL
import           Control.Monad

import           Data.Conduit
import           Data.Conduit.Network
import           Data.Conduit.List as CL

import           Codec.Compression.Zlib
import           Control.Concurrent.STM

import           Control.Monad.Reader

main :: IO ()
main = do
    BS.putStrLn "Loading database..."
    db         <- loadStateFromPath "testDB/"
    BS.putStrLn "Building cache..."
    startCache <- constructStartCache db
    tvi        <- newTVarIO startCache
    BS.putStrLn "Launching server..."
    flip runReaderT (ServerData db tvi) $ do
                runTCPServer (serverSettings 5002 "*4") $ listener
    closeAcidState db

listener :: Application ServerApplication
listener appData = src $= (documentConvert emptyGet) $= operationConduit $$ sink
        where src  = appSource appData
              sink = appSink appData
            
operationConduit :: Conduit B.Document ServerApplication BS.ByteString
operationConduit = awaitForever handleOperation
        where handleOperation doc = case (B.lookup "operation" doc) of
                Just (UFOperation uft opts) -> do
                    bs_response <- lift $ do
                        res <- case uft of
                            UFPut    -> do if (fromMaybe False $ B.lookup "unconfirmedWrite" opts)
                                              then do curServer <- ask
                                                      pl        <- B.lookup "payload" opts
                                                      void $ liftIO $ forkIO $ void $ flip runReaderT curServer $ insertNewDocument pl
                                              else insertNewDocument =<< B.lookup "payload" opts
                                           return $ UFResponse UFSuccess []
                            UFGet    -> getById =<< B.lookup "id" opts
                            UFFilter -> filterByFieldEval =<< B.lookup "parameters" opts
                        return $ BSL.toStrict $ runPut $ putDocument $ buildResponse res
                    yield bs_response
                Nothing   -> liftIO $ print "Error: no operation found."