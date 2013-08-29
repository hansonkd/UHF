{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell, DeriveGeneric, OverloadedStrings #-}
module UFdb.Types where

import           Data.Acid
import           Data.SafeCopy
import           Data.Typeable (Typeable)
import qualified Data.Bson as B
import           Data.Bson.Generic
import qualified Data.ByteString as BS
import           GHC.Generics
import qualified Data.Text as T
import qualified Data.Map as M
import           Data.Set (Set)
import           Control.Monad.Reader
import           Control.Concurrent.STM

$(deriveSafeCopy 0 'base ''B.Binary)
$(deriveSafeCopy 0 'base ''B.Function)
$(deriveSafeCopy 0 'base ''B.UUID)
$(deriveSafeCopy 0 'base ''B.MD5)
$(deriveSafeCopy 0 'base ''B.UserDefined)
$(deriveSafeCopy 0 'base ''B.ObjectId)
$(deriveSafeCopy 0 'base ''B.Regex)
$(deriveSafeCopy 0 'base ''B.Javascript)
$(deriveSafeCopy 0 'base ''B.Symbol)
$(deriveSafeCopy 0 'base ''B.MongoStamp)
$(deriveSafeCopy 0 'base ''B.MinMaxKey)
$(deriveSafeCopy 0 'base ''B.Value)
$(deriveSafeCopy 0 'base ''B.Field)

data UFDocument = UFDocument { documentKey :: B.ObjectId, documentData :: B.Document }
    deriving (Generic, Eq, Ord, Typeable, Show)
instance ToBSON UFDocument
instance FromBSON UFDocument
$(deriveSafeCopy 0 'base ''UFDocument)

-- | basically we just want to flatten  our document and then have our labels show their parents
-- | e.g [child : [count : 4], someField : 9] becomes [ child.count : 4, somefield : 9 ]
buildFieldIndex :: Maybe B.Label -> [B.Field] -> [B.Field]
buildFieldIndex _ ([])                                          = []
buildFieldIndex Nothing    (df@(fl B.:= (B.Doc docField)):docs) = (buildFieldIndex (Just fl) docField) ++ (buildFieldIndex Nothing docs)
buildFieldIndex (Just pl)  (df@(fl B.:= (B.Doc docField)):docs) = (flip buildFieldIndex docField (Just $ T.concat [pl, ".", fl])) ++ 
                                                (buildFieldIndex (Just pl) docs)
buildFieldIndex Nothing (field:docs)   = (field):(buildFieldIndex Nothing docs)
buildFieldIndex (Just pl) (field:docs) = (field {B.label = T.concat [pl, ".", B.label field]}):(buildFieldIndex (Just pl) docs)
                   

data Database = Database { documents :: (M.Map B.ObjectId BS.ByteString) }
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''Database)

data DocumentIndex = DocumentIndex { fieldIndex :: M.Map B.Field (Set B.ObjectId)
                                   } deriving (Generic, Eq, Ord, Typeable, Show)
                                   

$(deriveSafeCopy 0 'base ''DocumentIndex)
                                
emptyDocIndex = DocumentIndex $ M.empty

data ServerData = ServerData { acidDB :: AcidState Database, docIndex :: TVar (DocumentIndex) }

type ServerApplication = ReaderT ServerData IO

data UFOperationType = UFPut | UFGet | UFFilter
  deriving (Generic, Show, Typeable, Eq)
instance ToBSON UFOperationType
instance FromBSON UFOperationType

data UFOperation = UFOperation { operationType :: UFOperationType, operationOptions :: B.Document }
  deriving (Generic, Show, Typeable, Eq)
instance ToBSON UFOperation
instance FromBSON UFOperation

data UFResponseType = UFSuccess | UFFailure
  deriving (Generic, Show, Typeable, Eq)
instance ToBSON UFResponseType
instance FromBSON UFResponseType

data UFResponse = UFResponse { responseType :: UFResponseType, responseContent :: [B.Binary] }
  deriving (Generic, Show, Typeable, Eq)
instance ToBSON UFResponse
instance FromBSON UFResponse

$(deriveSafeCopy 0 'base ''UFOperationType)
$(deriveSafeCopy 0 'base ''UFOperation)
$(deriveSafeCopy 0 'base ''UFResponseType)
$(deriveSafeCopy 0 'base ''UFResponse)