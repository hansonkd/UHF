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

import           Data.ByteString as BS
import           Data.ByteString.Lazy as BSL

import           Control.Monad

import           Data.Conduit
import           Data.Conduit.Network
import           Data.Conduit.List as CL

main :: IO ()
main = do
    db   <- loadStateFromPath "testDB/"
    runTCPServer (serverSettings 5002 "*4") $ loop db
    closeAcidState db


loop :: AcidState Database -> Application IO
loop db appData = let src = appSource appData
                      sink = appSink appData
                  in src $= (documentConvert $ runGetIncremental getDocument) $= (handleOperation db) $$ sink
        

documentBuilde
documentConvert :: MonadIO m => Decoder B.Document -> Conduit BS.ByteString m B.Document
documentConvert built = case built of
                                Done _ _ doc -> do yield doc
                                                   documentConvert $ runGetIncremental getDocument
                                Partial _    -> awaitForever $ (\msg -> documentConvert (pushChunk built msg))
                                Fail _ _ err -> do
                                    liftIO $ print err
                                    yield (buildResponse $ UFResponse UFFailure [])
                                    documentConvert $ runGetIncremental getDocument


operationConduit :: MonadIO m => AcidState Database -> Conduit B.Document m BS.ByteString
operationConduit db = awaitForever handleOperation
    where handleOperation doc = case (B.lookup "operation" doc) of
                        Just (UFOperation uft opts) -> do
                            bs_response <- liftIO $ do
                                res <- case uft of
                                    UFPut    -> do insertNewDocument db =<< B.lookup "payload" opts
                                                   return $ UFResponse UFSuccess []
                                    UFGet    -> getById db =<< B.lookup "id" opts
                                    UFFilter -> filterByFieldEval db =<< B.lookup "parameters" opts
                                return $ BS.concat $ BSL.toChunks $ runPut $ putDocument $ buildResponse res
                            yield bs_response
                        Nothing   -> liftIO $ print "Error: no operation found."
                    
