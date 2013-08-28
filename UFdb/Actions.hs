{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}

module UFdb.Actions where

import           Control.Monad.State ( get, put )
import           Control.Monad.Reader ( ask )
import           Custom.IxSet ((@=))
import qualified Custom.IxSet as IxSet
import qualified Data.Bson as B
import           UFdb.Types
import           Data.Bson.Binary
import           Data.Binary.Put (runPut)
import           Data.Binary.Get (runGet)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Bson.Generic
import           Data.Acid
import           Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map.Strict as M

addDocument :: B.ObjectId -> BS.ByteString -> Update Database ()
addDocument docKey docData
    = do d@Database{..} <- get
         put $ Database $ M.insert docKey docData documents

unwrapDB :: Query Database (M.Map B.ObjectId BS.ByteString)
unwrapDB = do d@Database{..} <- ask
              return documents
         
viewDocuments :: Int -> Query Database UFResponse
viewDocuments limit
    = do d@Database{..} <- ask
         return $ UFResponse UFSuccess $ fmap (B.Binary) $ take limit $ M.elems documents

viewDocumentById :: B.ObjectId -> Query Database UFResponse
viewDocumentById objid 
    = do d@Database{..} <- ask
         case (M.lookup objid documents) of
            Just doc -> return $ UFResponse UFSuccess [B.Binary doc]
            Nothing  -> return $ UFResponse UFFailure []
            
-- | Operations using our parsed 
viewDocumentByField :: B.Field -> IxSet.IxSet UFDocument -> Query Database UFResponse
viewDocumentByField field indexed
    = do d@Database{..} <- ask
         case (IxSet.getOne $ indexed @= field) of
            Just doc -> return $ UFResponse UFSuccess $ fmap B.Binary $ fromMaybe [] $ fmap (flip (:) []) $ M.lookup (documentKey doc) documents
            Nothing  -> return $ UFResponse UFFailure []

setOperation :: B.Label -> [B.Field] -> (IxSet.IxSet UFDocument -> IxSet.IxSet UFDocument -> IxSet.IxSet UFDocument) -> IxSet.IxSet UFDocument -> IxSet.IxSet UFDocument
setOperation funcLabel funcDoc func documents = fromMaybe IxSet.empty $ do
                            funcParams <- B.lookup funcLabel funcDoc :: Maybe [B.Field]
                            arg1       <- B.lookup "arg1" funcParams :: Maybe [B.Field]
                            arg2       <- B.lookup "arg2" funcParams :: Maybe [B.Field]
                            Just (func (parseAll arg1 documents) (parseAll arg2 documents)) 

filterByField :: B.Label -> [B.Field] -> (B.Field -> B.Field -> Bool) -> IxSet.IxSet UFDocument -> IxSet.IxSet UFDocument
filterByField funcLabel funcDoc func documents = fromMaybe IxSet.empty $ do
                            funcParams <- B.lookup funcLabel funcDoc :: Maybe [B.Field]
                            label      <- B.lookup "label" funcParams
                            val        <- B.look "value" funcParams
                            Just (IxSet.filterByKey (\f -> (B.label f == label) && (func f (label B.:= val))) documents) 

parseAll :: [B.Field] -> IxSet.IxSet UFDocument -> IxSet.IxSet UFDocument
parseAll funcDoc documents = let setResults = parseSetOps funcDoc documents
                                 ordResults = parseOrdOps funcDoc documents
                             in foldr IxSet.union IxSet.empty [setResults, ordResults]

parseSetOps :: [B.Field] -> IxSet.IxSet UFDocument -> IxSet.IxSet UFDocument
parseSetOps funcDoc documents = let unionResults        = setOperation "$union" funcDoc IxSet.union documents
                                    intersectionResults = setOperation "$intersection" funcDoc IxSet.intersection documents
                                in foldr IxSet.union IxSet.empty [unionResults, intersectionResults]

parseOrdOps :: [B.Field] -> IxSet.IxSet UFDocument -> IxSet.IxSet UFDocument
parseOrdOps funcDoc documents = let ltResults = filterByField "$LT" funcDoc (<) documents
                                    gtResults = filterByField "$GT" funcDoc (>) documents
                                    eResults  = filterByField "$EQ" funcDoc (==) documents
                                in foldr IxSet.union IxSet.empty [ltResults, gtResults, eResults]
                                
viewDocumentsByFieldEval :: [B.Field] -> Int -> IxSet.IxSet UFDocument -> Query Database UFResponse
viewDocumentsByFieldEval func limit indexed
    = do d@Database{..} <- ask
         return $ UFResponse UFSuccess $ fmap (B.Binary) $ catMaybes $ fmap (\d -> M.lookup (documentKey d) documents) $ 
                 take limit $ IxSet.toList $ parseAll func indexed
                                      
$(makeAcidic ''Database ['addDocument, 'unwrapDB, 'viewDocuments, 'viewDocumentById, 'viewDocumentByField, 'viewDocumentsByFieldEval])

loadStateFromPath :: FilePath -> IO (AcidState Database)
loadStateFromPath fp = openLocalStateFrom fp (Database (M.empty))

insertNewDocument :: AcidState Database -> BS.ByteString -> IxSet.IxSet UFDocument -> B.Document -> IO (IxSet.IxSet UFDocument)
insertNewDocument database serialized indexed document = do
    nextKey <- B.genObjectId
    update database (AddDocument nextKey serialized)
    return (IxSet.insert (UFDocument nextKey document) indexed)

getById :: AcidState Database -> B.ObjectId -> IO UFResponse
getById database objid = do
    query database (ViewDocumentById objid)
         
filterByFieldEval :: AcidState Database -> IxSet.IxSet UFDocument -> [B.Field] -> IO UFResponse
filterByFieldEval database indexed func = do
    query database (ViewDocumentsByFieldEval func 10 indexed)
    
constructStartCache :: AcidState Database -> IO (IxSet.IxSet UFDocument)
constructStartCache db = do
            raw_data <- query db UnwrapDB
            return $ M.foldrWithKey (\k v i -> IxSet.insert (UFDocument k (runGet getDocument $ BL.fromStrict v)) i) IxSet.empty raw_data
    
buildResponse :: UFResponse -> B.Document
buildResponse r = [ "response" B.:= (B.Doc $ toBSON $ r) ]