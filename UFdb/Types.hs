{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
module UFdb.Types where

import           Data.Acid
import           Data.SafeCopy
import           Data.Typeable (Typeable)
import           Custom.IxSet ( Indexable(..), IxSet(..), ixFun, ixSet )
import qualified Data.Bson as B
import           Data.Bson.Generic
import           GHC.Generics

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

instance Indexable UFDocument where
     empty = ixSet [ ixFun $ \ufd -> [ documentKey ufd ]
                   , ixFun $ \ufd -> documentData ufd -- | Index all the fields 
                   ]

data Database = Database { documents :: IxSet UFDocument }
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''Database)

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

data UFResponse = UFResponse { responseType :: UFResponseType, responseContent :: [UFDocument] }
  deriving (Generic, Show, Typeable, Eq)
instance ToBSON UFResponse
instance FromBSON UFResponse

$(deriveSafeCopy 0 'base ''UFOperationType)
$(deriveSafeCopy 0 'base ''UFOperation)
$(deriveSafeCopy 0 'base ''UFResponseType)
$(deriveSafeCopy 0 'base ''UFResponse)