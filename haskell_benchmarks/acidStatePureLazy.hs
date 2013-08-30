{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment
import Data.List (replicate)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text as T

import           Control.Concurrent (threadDelay)

import qualified Data.Map.Lazy as Map

import qualified Data.Bson as B
import           Data.Binary.Put
import           Data.Bson.Binary
import qualified Data.Set as S

import           Control.Monad

type Key = Int
type Value = BL.ByteString

data KeyValue = KeyValue (Map.Map Key Value)

baseDoc :: [Double] -> Double -> B.Document
baseDoc arr x = ["test20" B.:= B.Array [B.Doc [ incFieldName B.:= B.Array [], "test21" B.:= B.Float x, "test22" B.:= B.Array (map B.Float arr)]], "test21" B.:= B.Float x, "test22" B.:= B.Array (map B.Float arr)]
            where incFieldName = "test" `T.append` (T.pack $ show x)
            
littleDoc x = baseDoc [] x
bigDoc x = baseDoc [0..5000] x

convertToBS :: B.Document -> Value
convertToBS x = runPut $ putDocument x

sourceConverted :: [(Key, Value)]
sourceConverted = zip [0..] $ map (convertToBS . bigDoc) [0..1000]

sourceNormal :: [(Key, Value)]
sourceNormal = zip [0..] $ map (\x -> BL.concat $ replicate 10000 $ "alsfdd" `CL.append` (CL.pack $ show x)) [0..10000]

main :: IO ()
main = do args <- getArgs
          --pureMap
          case args of
              (a:_)        -> run sourceNormal
              otherwise    -> run sourceConverted
      where run source = do 
                 putStrLn "Putting 1000 Lazy ByteStrings into a Map"
                 let newMap = foldr (\(k,v) i -> Map.insert k v $! i) Map.empty source
                 putStrLn "Done..."
                 putStrLn "Launching interactive mode"
                 forever $ do
                      putStrLn "Enter an integer:"
                      k <- getLine
                      print $ fmap BL.length $ Map.lookup (read k) newMap


