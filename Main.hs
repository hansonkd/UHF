{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric          #-}

module Main where

import           UFdb.Types
import           UFdb.Actions

import           Control.Monad.Trans (liftIO, MonadIO)
import           Data.Acid
import           Data.Maybe (fromMaybe)
import qualified Data.Bson as B
import           Data.Bson.Generic
import           Data.Bson.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import qualified Custom.IxSet as IxSet

import           Data.ByteString.Char8 as BS
import           Data.ByteString.Lazy as BSL

import           Control.Monad

import           Data.Conduit
import           Data.Conduit.Network
import           Data.Conduit.List as CL

import           Codec.Compression.Zlib
import           Control.Concurrent.STM



emptyGet :: Decoder B.Document
emptyGet = runGetIncremental getDocument

main :: IO ()
main = do
    BS.putStrLn "Loading database..."
    db         <- loadStateFromPath "testDB/"
    BS.putStrLn "Building cache..."
    startCache <- (constructStartCache db)
    tvi        <- newTVarIO startCache
    BS.putStrLn "Launching server..."
    runTCPServer (serverSettings 5002 "*4") $ listener db tvi
    closeAcidState db

listener :: AcidState Database -> TVar (IxSet.IxSet UFDocument) -> Application IO
listener db tvi appData = src $= (documentConvert emptyGet) $= (operationConduit db tvi) $$ sink
        where src  = appSource appData
              sink = appSink appData

documentConvert :: MonadIO m => Decoder B.Document -> Conduit BS.ByteString m B.Document
documentConvert built = await >>= maybe (return ()) handleConvert
    where handleConvert msg = do
                        let newMsg = pushChunks built $ decompress $ BSL.fromStrict msg
                        case newMsg of
                                Done a n doc -> do yield doc
                                                   documentConvert $ pushChunk emptyGet a
                                Partial _    -> documentConvert newMsg
                                Fail a _ err -> do
                                    liftIO $ print err
                                    yield $ buildResponse $ UFResponse UFFailure []
                                    documentConvert $ pushChunk emptyGet a
            
operationConduit :: MonadIO m => AcidState Database -> TVar (IxSet.IxSet UFDocument) -> Conduit B.Document m BS.ByteString
operationConduit db tvi = awaitForever handleOperation
        where handleOperation doc = case (B.lookup "operation" doc) of
                Just (UFOperation uft opts) -> do
                    indexed <- liftIO $ readTVarIO tvi
                    bs_response <- liftIO $ do
                        res <- case uft of
                            UFPut    -> do new_index <- (\x -> insertNewDocument db (BSL.toStrict $ runPut $ putDocument x) indexed x) =<< B.lookup "payload" opts
                                           liftIO $ atomically $ writeTVar tvi new_index
                                           return $ UFResponse UFSuccess []
                            UFGet    -> getById db =<< B.lookup "id" opts
                            UFFilter -> filterByFieldEval db indexed =<< B.lookup "parameters" opts
                        return $ BSL.toStrict $ runPut $ putDocument $ buildResponse res
                    yield bs_response
                Nothing   -> liftIO $ print "Error: no operation found."