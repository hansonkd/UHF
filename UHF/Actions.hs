{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}

module UHF.Actions where

import           UHF.Types

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import           Control.Exception (evaluate)
import           Control.Monad.Trans (liftIO, MonadIO, lift)
import           Control.Monad.State ( get, put )
import           Control.Monad.Reader ( ask, asks, runReaderT )
import           Control.Monad (void)

import           Data.Acid

import qualified Data.Bson as B
import           Data.Bson.Binary
import           Data.Bson.Generic
import           Data.Binary.Get
import           Data.Binary.Put hiding (Put)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy  as BSL

import           Data.Conduit
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import qualified Data.Text as T
import           Data.List (foldl')
import           System.IO.Unsafe (unsafePerformIO)


-- | Stuff that should go into Internal
-- | 
emptyGet :: Decoder B.Document
emptyGet = runGetIncremental getDocument

-- | basically we just want to flatten  our document and then have our labels show their parents
-- | e.g [child : [count : 4], someField : 9] becomes [ child.count : 4, somefield : 9 ]
buildFieldIndex :: Maybe B.Label -> [B.Field] -> [B.Field]
buildFieldIndex _ ([])                                          = []
buildFieldIndex Nothing    (df@(fl B.:= (B.Doc docField)):docs) = (buildFieldIndex (Just fl) docField) ++ (buildFieldIndex Nothing docs)
buildFieldIndex (Just pl)  (df@(fl B.:= (B.Doc docField)):docs) = (flip buildFieldIndex docField (Just $ T.concat [pl, ".", fl])) ++ 
                                                (buildFieldIndex (Just pl) docs)
buildFieldIndex Nothing (field:docs)   = (field):(buildFieldIndex Nothing docs)
buildFieldIndex (Just pl) (field:docs) = (field {B.label = T.concat [pl, ".", B.label field]}):(buildFieldIndex (Just pl) docs)

buildIndex :: B.ObjectId -> BS.ByteString -> DocumentIndex -> DocumentIndex
buildIndex objId serialized docIndex@DocumentIndex{..} = DocumentIndex $ newFieldIndex
     where conv   = pushChunk emptyGet serialized
           doc    = case conv of
                       Done _ _ d -> d
                       otherwise  -> []
           fields = buildFieldIndex Nothing $ doc
           newFieldIndex = foldl' update fieldIndex fields
               where update fi field = M.insertWith Set.union field (Set.singleton objId) fi


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
                                    documentConvert $ pushChunk emptyGet a
-- | Acidstate stuff
addDocument :: B.ObjectId -> BS.ByteString -> Update Database ()
addDocument docKey docData
    = do d@Database{..} <- get
         put $ Database $ M.insert docKey docData documents

replaceDocument :: B.ObjectId -> BS.ByteString -> Update Database ()
replaceDocument docKey docData
    = do d@Database{..} <- get
         put $ Database $ M.update (\_ -> Just docData) docKey documents
         
unwrapDB :: Query Database (M.Map B.ObjectId BS.ByteString)
unwrapDB = do d@Database{..} <- ask
              return documents
         
viewDocuments :: Int -> Query Database Response
viewDocuments limit
    = do d@Database{..} <- ask
         return $ Response Success $ fmap B.Binary $ take limit $ M.elems documents -- (B.Binary . BL.toStrict)

viewDocumentById :: B.ObjectId -> Query Database Response
viewDocumentById objid 
    = do d@Database{..} <- ask
         case (M.lookup objid documents) of
            Just doc -> return $ Response Success [B.Binary doc]
            Nothing  -> return $ Response Failure []
            
-- | Operations using our parsed 
viewDocumentByField :: B.Field -> DocumentIndex -> Query Database Response
viewDocumentByField field indexed
    = do d@Database{..} <- ask
         case (M.lookup field (fieldIndex indexed)) of
            Just docSet -> return $ Response Success $ map B.Binary $ catMaybes $ map (\docId -> M.lookup docId documents) $ Set.toAscList docSet
            Nothing     -> return $ Response Failure []

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
                            Just $ foldl' Set.union Set.empty $ M.elems filteredMap

parseAll :: [B.Field] -> DocumentIndex -> Set B.ObjectId
parseAll funcDoc documents = let setResults = parseSetOps funcDoc documents
                                 ordResults = parseOrdOps funcDoc documents
                             in foldl' Set.union Set.empty [setResults, ordResults]

parseSetOps :: [B.Field] -> DocumentIndex -> Set B.ObjectId
parseSetOps funcDoc documents = let unionResults        = setOperation "$union" funcDoc Set.union documents
                                    intersectionResults = setOperation "$intersection" funcDoc Set.intersection documents
                                in foldl' Set.union Set.empty [unionResults, intersectionResults]

parseOrdOps :: [B.Field] -> DocumentIndex -> Set B.ObjectId
parseOrdOps funcDoc documents = let ltResults = filterByField "$LT" funcDoc (<) documents
                                    gtResults = filterByField "$GT" funcDoc (>) documents
                                    eResults  = filterByField "$EQ" funcDoc (==) documents
                                in foldl' Set.union Set.empty [ltResults, gtResults, eResults]
                                
viewDocumentsByFieldEval :: [B.Field] -> Int -> DocumentIndex -> Query Database Response
viewDocumentsByFieldEval func limit indexed
    = do d@Database{..} <- ask
         return $ Response Success $ fmap B.Binary $ catMaybes $ fmap (\d -> M.lookup d documents) $ 
                 take limit $ Set.toAscList $ parseAll func indexed
                                      
$(makeAcidic ''Database ['addDocument, 'replaceDocument, 'unwrapDB, 'viewDocuments, 'viewDocumentById, 'viewDocumentByField, 'viewDocumentsByFieldEval])


-- | Interface for server.
loadStateFromPath :: FilePath -> IO (AcidState Database)
loadStateFromPath fp = openLocalStateFrom fp (Database (M.empty))

insertNewDocument :: B.Binary -> ServerApplication ()
insertNewDocument (B.Binary serialized) = do
    db  <- asks acidDB
    tvi <- asks docIndex
    indexed <- liftIO $ readTVarIO tvi
    nextKey <- liftIO $ B.genObjectId
    liftIO $ do update db (AddDocument nextKey serialized)
                atomically $ writeTVar tvi $! buildIndex nextKey serialized indexed

updateDocument :: B.ObjectId -> B.Binary -> ServerApplication ()
updateDocument docKey (B.Binary serialized) = do
    db  <- asks acidDB
    tvi <- asks docIndex
    indexed <- liftIO $ readTVarIO tvi
    liftIO $ do update db (ReplaceDocument docKey serialized)
                atomically $ writeTVar tvi $! buildIndex docKey serialized indexed
                
getById :: B.ObjectId -> ServerApplication Response
getById objid = do
    db  <- asks acidDB
    liftIO $ query db (ViewDocumentById objid)
         
filterByFieldEval :: [B.Field] -> ServerApplication Response
filterByFieldEval func = do
    database <- asks acidDB
    indexed <-  asks docIndex >>= (liftIO . readTVarIO)
    liftIO $ query database (ViewDocumentsByFieldEval func 10 indexed)

constructStartCache :: AcidState Database -> IO (DocumentIndex)
constructStartCache db = do
            raw_data <- query db UnwrapDB
            return $! M.foldlWithKey' (\i k v -> buildIndex k v i) emptyDocIndex raw_data
    
buildResponse :: Response -> B.Document
buildResponse r = [ "response" B.:= (B.Doc $ toBSON $ r) ]


handleOperation :: Operation -> ServerApplication Response
handleOperation (Operation uft opts) = 
    do res <- case uft of
            Put    -> do if (fromMaybe False $ B.lookup "unconfirmedWrite" opts)
                              then do curServer <- ask
                                      pl        <- B.lookup "payload" opts
                                      void $ liftIO $ forkIO $ void $ flip runReaderT curServer $ insertNewDocument pl
                              else insertNewDocument =<< B.lookup "payload" opts
                         return $ Response Success []
            Update -> do docId <- B.lookup "id" opts
                         payload <- B.lookup "payload" opts
                         updateDocument docId payload
                         return $ Response Success []
            Get    -> getById =<< B.lookup "id" opts
            Filter -> filterByFieldEval =<< B.lookup "parameters" opts
       return res

operationConduit :: Conduit B.Document ServerApplication BS.ByteString
operationConduit = awaitForever handleDocument
        where handleDocument doc = case (B.lookup "operation" doc) of
                Just op -> do
                    bs_response <- lift $ handleOperation op
                    yield $ BSL.toStrict $ runPut $ putDocument $ buildResponse bs_response
                Nothing   -> liftIO $ print "Error: no operation found."