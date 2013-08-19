{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable     #-}

module Client where

import           Data.Maybe (fromMaybe)
import           Control.Monad (forM_)
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString.Lazy (recv, sendAll)

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

data TestBsonData = TestBsonData { test20 :: Maybe TestBsonData, test21 :: Int }
  deriving (Generic, Show, Typeable, Eq)
instance ToBSON TestBsonData
instance FromBSON TestBsonData


sampleData :: B.Document
sampleData = toBSON $ TestBsonData (Just $ TestBsonData Nothing 0) 4

serializedPut x = ["operation" B.:= (B.Doc $ toBSON $ UFOperation UFPut $ (["payload" B.:= (B.Doc $ toBSON $ TestBsonData (Just $ TestBsonData Nothing (x*10)) x)]))]

serializedGet = ["operation" B.:= (B.Doc $ toBSON $ UFOperation UFFilter (["parameters" B.:= (B.Doc $
                                         [ "$LT" B.:= (B.Doc $ 
                                             [ "label" B.:= B.String "test20.test21"
                                             , "value" B.:= B.Int32 100
                                             ])
                                         ])]))]
                                         
main :: IO ()
main = do   args <- getArgs
            print $ buildFieldIndex sampleData Nothing
            withSocketsDo $
             do addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "5002")
                let serveraddr = head addrinfos
                sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                print serializedGet
                if null args
                    then do putStrLn "Getting documents from server 1000 times..."
                            connect sock (addrAddress serveraddr)
                            timeIt $ forM_ [0..1] (\x -> do
                                sendAll sock $ runPut $ putDocument $ serializedGet
                                resp <- recv sock (1024 * 1024)
                                print $ runGet getDocument resp)
                    else do let (arg:_) = args
                            if (arg == "local")
                                then do putStrLn "Puting 1000 documents directly to database..."
                                        db   <- loadStateFromPath "testDB/"
                                        timeIt $ forM_ [0..1000] (\x -> insertNewDocument db ["value" B.:= (B.Float x)])
                                else do putStrLn "Puting 1000 documents to server..."
                                        connect sock (addrAddress serveraddr)
                                        timeIt $ forM_ [0..1000] (\x -> do
                                            sendAll sock $ runPut $ putDocument $ serializedPut x
                                            recv sock 1024 >>= (\x -> return ())) 
                sClose sock
                putStrLn "Done!"