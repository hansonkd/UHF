{-# LANGUAGE OverloadedStrings      #-}

module Main where

import           Data.Maybe (fromMaybe)
import           Control.Monad (void, forM_)
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString.Lazy (recv, sendAll)

import qualified Data.Bson as B

import           Data.Binary.Get (runGet)
import           Data.Binary.Put (runPut)
import           Data.Bson.Binary

import           System.Environment ( getArgs )

import           Control.Concurrent (threadDelay)

import           UFdb.BenchmarksCommon

main :: IO ()
main = do   args <- getArgs
            withSocketsDo $
             do addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "5002")
                let serveraddr = head addrinfos
                
                sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                connect sock (addrAddress serveraddr)
                
                putStrLn "Putting 1000 small documents to server individually..."
                timeIt $ forM_ testRangeInsert (\x -> do
                    sendAll sock $ runPut $ putDocument $ serializedPutSmall x
                    sendAll sock "\n<hfEnd>"
                    void $ recv sock 1024)
                putStrLn "Done. \nPutting 1000 big documents to server individually..."
                sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                connect sock (addrAddress serveraddr)
                timeIt $ forM_ testRangeInsert (\x -> do
                    sendAll sock $ runPut $ putDocument $ serializedPutBig x
                    void $ send sock "\n<hfEnd>"
                    void $ recv sock 1024)
                
                sendAll sock $ runPut $ putDocument $ serializedGetLT
                void $ send sock "\n<hfEnd>"
                result <- recv sock (1024 * 1024)
                putStrLn $ "Results from query: "
                print $ runGet getDocument result
                
                threadDelay $ 1000000
                putStrLn "Done Putting \nGetting documents from server 100 times..."
                timeIt $ forM_ testRangeSearch (\x -> do
                    sendAll sock $ runPut $ putDocument $ serializedGetUnion x
                    void $ send sock "\n<hfEnd>"
                    void $ recv sock (1024 * 1024))
                sClose sock
                putStrLn "Done!"