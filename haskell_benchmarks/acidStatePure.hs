{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, BangPatterns, OverloadedStrings #-}
module Main (main) where

import System.Environment
import Data.List (replicate)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString.Char8 as C
import qualified Data.Text             as T

import           Control.Concurrent (threadDelay)

import qualified Data.Map.Strict      as Map

import qualified Data.Bson            as B
import           Data.Binary.Put
import           Data.Bson.Binary
import qualified Data.Set             as S

import           Control.Monad

type Key = Int
type Value = BS.ByteString

data KeyValue = KeyValue (Map.Map Key Value)

baseDoc :: [Double] -> Double -> B.Document
baseDoc arr x = ["test20" B.:= B.Float x, incFieldName B.:= B.Float x, "test22" B.:= B.Array (map B.Float arr)]
            where incFieldName = "test" `T.append` (T.pack $ show x)
            
littleDoc x = baseDoc [] x
bigDoc x = baseDoc [0..5000] x

convertToBS :: B.Document -> Value
convertToBS x = BL.toStrict $ runPut $ putDocument x

sourceConverted :: [(Key, Value)]
sourceConverted = zip [0..] $ map (convertToBS . bigDoc) [0..1000]

sourceNormal :: [(Key, Value)]
sourceNormal = zip [0..] $ map (\x -> BS.concat $ replicate 10000 $ "alsfdd" `C.append` (C.pack $ show x)) [0..10000]

main :: IO ()
main = do args <- getArgs
          case args of
              (a:_)        -> run sourceNormal
              otherwise    -> run sourceConverted
      where run source = do 
                 putStrLn "Putting 1000 Lazy ByteStrings into a Map"
                 --newMap <- foldM  (\i (k, v) -> (print $ Map.size i) >> (return Map.insert k v i)) Map.empty source
                 let newMap = foldr (\(k,v) i -> Map.insert k v i) Map.empty source
                 putStrLn "Done..."
                 putStrLn "Launching interactive mode"
                 forever $ do
                      putStrLn "Enter an integer:"
                      k <- getLine
                      print $ Map.lookup (read k) newMap

