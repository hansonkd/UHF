{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable     #-}

module Client where

import           Data.Maybe (fromMaybe)
import           Control.Monad (void, forM_)
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString.Lazy (recv, sendAll)
import qualified Network.Socket.ByteString as SBS

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Bson as B
import           Data.Bson.Generic

import           GHC.Generics
import           Data.Binary.Get (runGet)
import           Data.Binary.Put (runPut)
import           Data.Bson.Binary

import           Data.Acid
import           UFdb.Types
import           UFdb.Actions
import           System.Environment ( getArgs )
import           Data.Typeable

import           System.TimeIt

data TestBsonData = TestBsonData { test20 :: Maybe TestBsonData, test21 :: Double, test22 :: [Double] }
  deriving (Generic, Show, Typeable, Eq)
instance ToBSON TestBsonData
instance FromBSON TestBsonData

serializedPutSmall x = ["operation" B.:= (B.Doc $ toBSON $ UFOperation UFPut $ (["payload" B.:= (B.Doc $ toBSON $ TestBsonData (Just $ TestBsonData Nothing (x*10) []) x [])]))]

serializedPutBig x = ["operation" B.:= (B.Doc $ toBSON $ UFOperation UFPut $ (["payload" B.:= (B.Doc $ toBSON $ TestBsonData (Just $ TestBsonData Nothing (x*10) [1..500]) x [1..500])]))]

serializedGetLT = ["operation" B.:= (B.Doc $ toBSON $ UFOperation UFFilter (["parameters" B.:= (B.Doc $
                                         [ "$LT" B.:= (B.Doc $ 
                                             [ "label" B.:= B.String "test20.test21"
                                             , "value" B.:= B.Int32 100
                                             ])
                                         ])]))]
                                         
serializedGetUnion = ["operation" B.:= (B.Doc $ toBSON $ UFOperation UFFilter (["parameters" B.:= (B.Doc $
                                         [ "$union" B.:= (B.Doc $ 
                                             [ "arg1" B.:= B.Doc ["$LT" B.:= B.Doc ["label" B.:= B.String "test20.test21", "value" B.:= B.Int32 100]]
                                             , "arg2" B.:= B.Doc ["$GT" B.:= B.Doc ["label" B.:= B.String "test21", "value" B.:= B.Int32 5]]
                                             ])
                                         ])]))]
testRangeInsert :: [Double]
testRangeInsert = [0..1000]

testRangeSearch :: [Double]
testRangeSearch = [0..100]

main :: IO ()
main = do   args <- getArgs
            withSocketsDo $
             do addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "5002")
                let serveraddr = head addrinfos
                
                sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                connect sock (addrAddress serveraddr)
                
                putStrLn "Puting 1000 small documents to server individually..."
                timeIt $ forM_ testRangeInsert (\x -> do
                    sendAll sock $ runPut $ putDocument $ serializedPutSmall x
                    sendAll sock "\n<hfEnd>"
                    void $ recv sock 1024)
                putStrLn "Done. \nPuting 1000 big documents to server individually..."
                sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                connect sock (addrAddress serveraddr)
                timeIt $ forM_ testRangeInsert (\x -> do
                    sendAll sock $ runPut $ putDocument $ serializedPutBig x
                    void $ send sock "\n<hfEnd>"
                    void $ recv sock 1024)
                putStrLn "Done Putting \nGetting documents from server 100 times..."
                timeIt $ forM_ testRangeSearch (\x -> do
                    sendAll sock $ runPut $ putDocument $ serializedGetUnion
                    void $ send sock "\n<hfEnd>"
                    void $ recv sock (1024 * 1024))
                
                sendAll sock $ runPut $ putDocument $ serializedGetUnion
                void $ send sock "\n<hfEnd>"
                result <- recv sock (1024 * 1024)
                putStrLn $ "Results from query: "
                print $ runGet getDocument result
                
                sClose sock
                putStrLn "Done!"