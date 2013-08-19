{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell, DeriveGeneric, OverloadedStrings #-}
module UFdb.Types where

import           Data.Acid
import           Data.SafeCopy
import           Data.Typeable (Typeable)
import           Custom.IxSet ( Indexable(..), IxSet(..), ixFun, ixSet )
import qualified Data.Bson as B
import           Data.Bson.Generic
import           GHC.Generics
import qualified Data.Text as T


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
buildFieldIndex :: [B.Field] -> Maybe B.Label -> [B.Field]
buildFieldIndex ([]) _                 = []
buildFieldIndex (df@(fl B.:= (B.Doc docField)):docs) Nothing   = (buildFieldIndex docField (Just $ fl)) ++ (buildFieldIndex docs Nothing)
buildFieldIndex (df@(fl B.:= (B.Doc docField)):docs) (Just pl) = (buildFieldIndex docField (Just $ T.concat [pl, ".", fl])) ++ 
                                                (buildFieldIndex docs (Just pl))
buildFieldIndex (field:docs) Nothing   = (field):(buildFieldIndex docs Nothing)
buildFieldIndex (field:docs) (Just pl) = (field {B.label = T.concat [pl, ".", B.label field]}):(buildFieldIndex docs (Just pl))

instance Indexable UFDocument where
     empty = ixSet [ ixFun $ \ufd -> [ documentKey ufd ]
                   , ixFun $ \ufd -> buildFieldIndex (documentData ufd) Nothing -- | Index all the fields 
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