{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import qualified Data.Map.Strict      as Map
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import           Control.Monad

type Key = Int
type Value = BS.ByteString

data KeyValue = KeyValue !(Map.Map Key Value)

source :: [(Key, Value)]
source = zip [0..] $ map (\x -> BS.concat $ replicate 10000 $ "alsfdd" `C.append` (C.pack $ show x)) [0..10000]

main = do
     putStrLn "Putting 10000 Strict ByteStrings into a Map"
     let !newMap = foldr (\(k,v) i -> Map.insert k v $! i) Map.empty source
     putStrLn "Done..."
     putStrLn "Launching interactive mode"
     forever $ do
          putStrLn "Enter an integer:"
          k <- getLine
          print $ Map.lookup (read k) newMap