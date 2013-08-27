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
import qualified Data.ByteString.Lazy as BS
import           Data.Bson.Generic
import           Data.Acid
import           Data.Maybe (fromMaybe)

addDocument :: B.ObjectId -> B.Document -> Update Database ()
addDocument docKey docData
    = do d@Database{..} <- get
         put $ Database $ IxSet.insert (UFDocument docKey docData) documents

viewDocuments :: Int -> Query Database UFResponse
viewDocuments limit
    = do d@Database{..} <- ask
         return $ UFResponse UFSuccess $ take limit $ IxSet.toList documents

viewDocumentById :: B.ObjectId -> Query Database UFResponse
viewDocumentById objid 
    = do d@Database{..} <- ask
         case (IxSet.getOne $ documents @= objid) of
            Just doc -> return $ UFResponse UFSuccess [doc]
            Nothing  -> return $ UFResponse UFFailure []

viewDocumentbyField :: B.Field -> Query Database UFResponse
viewDocumentbyField field
    = do d@Database{..} <- ask
         case (IxSet.getOne $ documents @= field) of
            Just doc -> return $ UFResponse UFSuccess [doc]
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
                                
viewDocumentsbyFieldEval :: [B.Field] -> Int -> Query Database UFResponse
viewDocumentsbyFieldEval func limit
    = do d@Database{..} <- ask
         return $ UFResponse UFSuccess $ take limit $ IxSet.toList $ parseAll func documents
                                      
$(makeAcidic ''Database ['addDocument, 'viewDocuments, 'viewDocumentbyField, 'viewDocumentsbyFieldEval, 'viewDocumentById])

loadStateFromPath :: FilePath -> IO (AcidState Database)
loadStateFromPath fp = openLocalStateFrom fp (Database (IxSet.empty))

insertNewDocument :: AcidState Database -> B.Document -> IO ()
insertNewDocument database document = do
    nextKey <- B.genObjectId
    update database (AddDocument nextKey document)

getById :: AcidState Database -> B.ObjectId -> IO UFResponse
getById database objid = do
    query database (ViewDocumentById objid)

filterByFieldEval :: AcidState Database -> [B.Field] -> IO UFResponse
filterByFieldEval database func = do
    query database (ViewDocumentsbyFieldEval func 10)

buildResponse :: UFResponse -> B.Document
buildResponse r = [ "response" B.:= (B.Doc $ toBSON $ r) ]