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

import           Codec.Compression.Zlib




emptyGet :: Decoder B.Document
emptyGet = runGetIncremental getDocument

main :: IO ()
main = do
    db   <- loadStateFromPath "testDB/"
    runTCPServer (serverSettings 5002 "*4") $ listener db
    closeAcidState db

listener :: AcidState Database -> Application IO
listener db appData = let src = appSource appData
                          sink = appSink appData
                      in src $= (documentConvert emptyGet) $= (operationConduit db) $$ sink
        


documentConvert :: MonadIO m => Decoder B.Document -> Conduit BS.ByteString m B.Document
documentConvert built = await >>= maybe (return ()) handleConvert
    where handleConvert msg = do
                        let newMsg = pushChunk built (BSL.toStrict $ decompress $ BSL.fromStrict msg)
                        case newMsg of
                                Done a _ doc -> do yield doc
                                                   documentConvert $ pushChunk emptyGet a
                                Partial _    -> documentConvert (pushChunk built msg)
                                Fail a _ err -> do
                                    liftIO $ print err
                                    yield (buildResponse $ UFResponse UFFailure [])
                                    documentConvert $ pushChunk emptyGet a
            
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
                    