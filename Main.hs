{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric          #-}

module Main where

import           UHF.Actions
import           UHF.Types
import           UHF.TeeVee.Application

import           Data.Acid

import           Data.ByteString.Char8 as BS
import           Data.Conduit
import           Data.Conduit.Network

import           Control.Concurrent.STM
import           Control.Concurrent
import           Control.Monad.Reader

main :: IO ()
main = do
    BS.putStrLn "Loading database..."
    db         <- loadStateFromPath "testDB/"
    BS.putStrLn "Building cache..."
    startCache <- constructStartCache db
    tvi        <- newTVarIO startCache
    let serverData = ServerData db tvi
    BS.putStrLn "Launching database server on port 5002..."
    forkIO $ flip runReaderT serverData $ do
                runTCPServer (serverSettings 5002 "*4") $ listener
    startServer serverData
    closeAcidState db

listener :: Application ServerApplication
listener appData = src $= (documentConvert emptyGet) $= operationConduit $$ sink
        where src  = appSource appData
              sink = appSink appData