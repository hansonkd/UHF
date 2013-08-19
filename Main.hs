{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric          #-}

module Main where

import           UFdb.Types
import           UFdb.Actions

import           Data.Acid
import           Data.Maybe (fromMaybe)
import qualified Data.Bson as B
import           Data.Bson.Generic
import           Data.Bson.Binary
import           Data.Binary.Get (runGet)

import           Data.ByteString.Lazy as BS

import           Network hiding (accept)
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString.Lazy (sendAll, recv)
import           Control.Concurrent
import           Control.Monad

main :: IO ()
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 5002
    db   <- loadStateFromPath "testDB/"
    loop sock db
    closeAcidState db

loop :: Socket -> AcidState Database -> IO ()
loop sock database = do
    (conn,_) <- accept sock
    forkIO $ body conn BS.empty
    loop sock database
    where body c built = do 
              msg <- recv c (1024)
              if (BS.length msg > 0)
                  then do
                      -- | Find a better way to check that server has sent all the data
                      let (mainPart, lastChunk) = BS.splitAt (BS.length msg - 7) msg
                      if (lastChunk == "<hfEnd>")
                          then do
                              let doc = runGet getDocument (built `BS.append` mainPart)
                              case (B.lookup "operation" doc) of
                                  Just ufop -> handleOperation database c ufop
                                  Nothing   -> print "Error" >> sendAll c (buildResponse $ UFResponse UFFailure [])
                              body c BS.empty
                          else do
                              body c (built `BS.append` msg)
                  else do
                      sClose c

handleOperation :: AcidState Database -> Socket -> UFOperation -> IO ()
handleOperation db c (UFOperation uft opts) = do
    res <- case uft of
        UFPut    -> do insertNewDocument db =<< B.lookup "payload" opts
                       return $ UFResponse UFSuccess []
        UFGet    -> getById db =<< B.lookup "id" opts
        UFFilter -> filterByFieldEval db =<< B.lookup "parameters" opts
    sendAll c $ buildResponse res