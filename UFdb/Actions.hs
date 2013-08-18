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

viewDocumentsbyFieldEval :: [B.Value] -> Int -> Query Database UFResponse
viewDocumentsbyFieldEval func limit
    = do d@Database{..} <- ask
         return $ UFResponse UFSuccess $ take limit $ IxSet.toList documents -- (IxSet.filterByKey (< field) documents)

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

filterByString :: AcidState Database -> [B.Value] -> IO UFResponse
filterByString database func = do
    query database (ViewDocumentsbyFieldEval func 10)

buildResponse :: UFResponse -> BS.ByteString
buildResponse r = runPut $ putDocument $ [ "response" B.:= (B.Doc $ toBSON $ r) ]