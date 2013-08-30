{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DeriveDataTypeable     #-}

module UFdb.BenchmarksCommon where
    
import           UFdb.Types

import           GHC.Generics
import           Data.Typeable

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text as T
import qualified Data.Bson as B
import           Data.Bson.Generic
import           Data.Binary.Put
import           Data.Bson.Binary


import           Data.Time
import           System.CPUTime
import           Text.Printf
import           System.TimeIt

import           Control.Monad.Trans (liftIO, MonadIO)

data TestBsonData = TestBsonData { test20 :: Maybe TestBsonData, test21 :: Double, test22 :: [Double] }
  deriving (Generic, Show, Typeable, Eq)
instance ToBSON TestBsonData
instance FromBSON TestBsonData

baseDoc :: [Double] -> Double -> B.Document
baseDoc arr x = ["test20" B.:= B.Array [B.Doc [ incFieldName B.:= B.Array [], "test21" B.:= B.Float x, "test22" B.:= B.Array (map B.Float arr)]], "test21" B.:= B.Float x, "test22" B.:= B.Array (map B.Float arr)]
            where incFieldName = "test" `T.append` (T.pack $ show x)
littleDoc x = baseDoc [] x
bigDoc x = baseDoc [0..500] x

convertToValue x = B.Bin $ B.Binary $ CL.toStrict $ runPut $ putDocument x

convertToPutOperation x = ["operation" B.:= (B.Doc $ toBSON $ UFOperation UFPut $ (["payload" B.:= (convertToValue $ x), "unconfirmedWrite" B.:= (B.Bool True)]))]

serializedPutSmall x = convertToPutOperation $ littleDoc x

serializedPutBig x   = convertToPutOperation $ bigDoc x

serializedGetLT    = ["operation" B.:= (B.Doc $ toBSON $ UFOperation UFFilter (["parameters" B.:= (B.Doc $
                                         [ "$LT" B.:= (B.Doc $ 
                                             [ "label" B.:= B.String "test20.test21"
                                             , "value" B.:= B.Int32 100
                                             ])
                                         ])]))]

serializedGetUnion :: Double -> B.Document                                        
serializedGetUnion x = ["operation" B.:= (B.Doc $ toBSON $ UFOperation UFFilter (["parameters" B.:= (B.Doc $
                                         [ "$union" B.:= (B.Doc $ 
                                             [ "arg1" B.:= B.Doc ["$LT" B.:= B.Doc ["label" B.:= B.String "test20.test21", "value" B.:= B.Float (x * 20)]]
                                             , "arg2" B.:= B.Doc ["$GT" B.:= B.Doc ["label" B.:= B.String "test21", "value" B.:= B.Float x]]
                                             ])
                                         ])]))]
timeIt :: MonadIO m => m a -> m a
timeIt opp = do
    t1 <- liftIO $ getCPUTime
    start <- liftIO $ getCurrentTime
    r <- opp
    stop <- liftIO $ getCurrentTime
    t2 <- liftIO $ getCPUTime
    let t :: Double
        t = fromIntegral (t2-t1) * 1e-12
    liftIO $ printf "CPU time: %6.2fs, User time: %s\n" t (show $ diffUTCTime stop start)
    return r

testRangeInsert :: [Double]
testRangeInsert = [0..1000]

testRangeSearch :: [Double]
testRangeSearch = [0..100]