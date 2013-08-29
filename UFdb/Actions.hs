{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}

module UFdb.Actions where

import           Control.Monad.State ( get, put )
import           Control.Monad.Reader ( ask )
import qualified Data.Bson as B
import           UFdb.Types
import           Data.Bson.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Bson.Generic
import           Data.Acid
import           Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map as M
import           Control.Monad.Reader
import           Control.Concurrent.STM
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           System.IO.Unsafe (unsafePerformIO)
import           Data.Conduit

buildIndex :: B.ObjectId -> BS.ByteString -> DocumentIndex -> DocumentIndex
buildIndex objId serialized docIndex@DocumentIndex{..} = DocumentIndex $ newFieldIndex
     where fields = buildFieldIndex Nothing $ runGet getDocument $ BL.fromStrict serialized
           newFieldIndex = foldr update fieldIndex fields
               where update field fi = M.insertWith Set.union field (Set.singleton objId) fi

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
         return $ UFResponse UFSuccess $ fmap B.Binary $ take limit $ M.elems documents -- (B.Binary . BL.toStrict)

viewDocumentById :: B.ObjectId -> Query Database UFResponse
viewDocumentById objid 
    = do d@Database{..} <- ask
         case (M.lookup objid documents) of
            Just doc -> return $ UFResponse UFSuccess [B.Binary doc]
            Nothing  -> return $ UFResponse UFFailure []
            
-- | Operations using our parsed 
viewDocumentByField :: B.Field -> DocumentIndex -> Query Database UFResponse
viewDocumentByField field indexed
    = do d@Database{..} <- ask
         case (M.lookup field (fieldIndex indexed)) of
            Just docSet -> return $ UFResponse UFSuccess $ map B.Binary $ catMaybes $ map (\docId -> M.lookup docId documents) $ Set.toAscList docSet
            Nothing     -> return $ UFResponse UFFailure []

setOperation :: B.Label -> [B.Field] -> (Set B.ObjectId -> Set B.ObjectId -> Set B.ObjectId) -> DocumentIndex -> Set B.ObjectId
setOperation funcLabel funcDoc func documents = fromMaybe Set.empty $ do
                            funcParams <- B.lookup funcLabel funcDoc :: Maybe [B.Field]
                            arg1       <- B.lookup "arg1" funcParams :: Maybe [B.Field]
                            arg2       <- B.lookup "arg2" funcParams :: Maybe [B.Field]
                            Just (func (parseAll arg1 documents) (parseAll arg2 documents)) 

filterByField :: B.Label -> [B.Field] -> (B.Field -> B.Field -> Bool) -> DocumentIndex -> Set B.ObjectId
filterByField funcLabel funcDoc func docIndex = fromMaybe Set.empty $ do
                            funcParams <- B.lookup funcLabel funcDoc :: Maybe [B.Field]
                            label      <- B.lookup "label" funcParams
                            val        <- B.look "value" funcParams
                            let filteredMap = (M.filterWithKey (\f _ -> (B.label f == label) && (func f (label B.:= val))) $ fieldIndex docIndex) 
                            Just $ foldr Set.union Set.empty $ M.elems filteredMap

parseAll :: [B.Field] -> DocumentIndex -> Set B.ObjectId
parseAll funcDoc documents = let setResults = parseSetOps funcDoc documents
                                 ordResults = parseOrdOps funcDoc documents
                             in foldr Set.union Set.empty [setResults, ordResults]

parseSetOps :: [B.Field] -> DocumentIndex -> Set B.ObjectId
parseSetOps funcDoc documents = let unionResults        = setOperation "$union" funcDoc Set.union documents
                                    intersectionResults = setOperation "$intersection" funcDoc Set.intersection documents
                                in foldr Set.union Set.empty [unionResults, intersectionResults]

parseOrdOps :: [B.Field] -> DocumentIndex -> Set B.ObjectId
parseOrdOps funcDoc documents = let ltResults = filterByField "$LT" funcDoc (<) documents
                                    gtResults = filterByField "$GT" funcDoc (>) documents
                                    eResults  = filterByField "$EQ" funcDoc (==) documents
                                in foldr Set.union Set.empty [ltResults, gtResults, eResults]
                                
viewDocumentsByFieldEval :: [B.Field] -> Int -> DocumentIndex -> Query Database UFResponse
viewDocumentsByFieldEval func limit indexed
    = do d@Database{..} <- ask
         return $ UFResponse UFSuccess $ fmap B.Binary $ catMaybes $ fmap (\d -> M.lookup d documents) $ 
                 take limit $ Set.toAscList $ parseAll func indexed
                                      
$(makeAcidic ''Database ['addDocument, 'unwrapDB, 'viewDocuments, 'viewDocumentById, 'viewDocumentByField, 'viewDocumentsByFieldEval])

loadStateFromPath :: FilePath -> IO (AcidState Database)
loadStateFromPath fp = openLocalStateFrom fp (Database (M.empty))

insertNewDocument :: B.Binary -> ServerApplication ()
insertNewDocument (B.Binary serialized) = do
    db  <- asks acidDB
    tvi <- asks docIndex
    indexed <- liftIO $ readTVarIO tvi
    nextKey <- liftIO $ B.genObjectId
    liftIO $ do let serialized' = BL.fromStrict serialized
                update db (AddDocument nextKey serialized)
                atomically $ writeTVar tvi $ buildIndex nextKey serialized indexed

getById :: B.ObjectId -> ServerApplication UFResponse
getById objid = do
    db  <- asks acidDB
    liftIO $ query db (ViewDocumentById objid)
         
filterByFieldEval :: [B.Field] -> ServerApplication UFResponse
filterByFieldEval func = do
    database <- asks acidDB
    indexed <-  asks docIndex >>= (liftIO . readTVarIO)
    liftIO $ query database (ViewDocumentsByFieldEval func 10 indexed)
    
constructStartCache :: AcidState Database -> IO (DocumentIndex)
constructStartCache db = do
            raw_data <- query db UnwrapDB
            return $ M.foldrWithKey (\k v i -> buildIndex k v i) emptyDocIndex raw_data
    
buildResponse :: UFResponse -> B.Document
buildResponse r = [ "response" B.:= (B.Doc $ toBSON $ r) ]

emptyGet :: Decoder B.Document
emptyGet = runGetIncremental getDocument

documentConvert :: Decoder B.Document -> Conduit BS.ByteString ServerApplication B.Document
documentConvert built = await >>= maybe (return ()) handleConvert
    where handleConvert msg = do
                        let newMsg = pushChunk built msg
                        case newMsg of
                                Done a n doc -> do yield doc
                                                   documentConvert $ pushChunk emptyGet a
                                Partial _    -> documentConvert newMsg
                                Fail a _ err -> do
                                    liftIO $ print err
                                    yield $ buildResponse $ UFResponse UFFailure []
                                    documentConvert $ pushChunk emptyGet a