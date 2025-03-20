-- | @\/v1\/vector_stores@
module OpenAI.V1.VectorStores
    ( -- * Main types
      VectorStoreID(..)
    , CreateVectorStore(..)
    , _CreateVectorStore
    , ModifyVectorStore(..)
    , _ModifyVectorStore
    , VectorStoreObject(..)

      -- * Other types
    , ExpiresAfter(..)
    , Status(..)

      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.AutoOr
import OpenAI.V1.ChunkingStrategy
import OpenAI.V1.DeletionStatus
import OpenAI.V1.Files (FileID)
import OpenAI.V1.Order
import OpenAI.V1.ListOf
import OpenAI.V1.VectorStores.FileCounts

-- | Vector store ID
newtype VectorStoreID = VectorStoreID{ text :: Text }
    deriving newtype (FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | The expiration policy for a vector store.
data ExpiresAfter = ExpiresAfter
    { anchor :: Text
    , days :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Request body for @\/v1\/vector_stores@
data CreateVectorStore = CreateVectorStore
    { file_ids :: Vector FileID
    , name :: Maybe Text
    , expires_after :: Maybe ExpiresAfter
    , chunking_strategy :: Maybe (AutoOr ChunkingStrategy)
    , metadata :: Maybe (Map Text Text)
    } deriving stock (Generic, Show)
      deriving (FromJSON, ToJSON)

-- | Default `CreateVectorStore`
_CreateVectorStore :: CreateVectorStore
_CreateVectorStore = CreateVectorStore
    { name = Nothing
    , expires_after = Nothing
    , chunking_strategy = Nothing
    , metadata = Nothing
    }

-- | Request body for @\/v1\/vector_stores\/:vector_store_id@
data ModifyVectorStore = ModifyVectorStore
    { name :: Maybe Text
    , expires_after :: Maybe ExpiresAfter
    , metadata :: Maybe (Map Text Text)
    } deriving stock (Generic, Show)
      deriving (FromJSON, ToJSON)

-- | Default `ModifyVectorStore`
_ModifyVectorStore :: ModifyVectorStore
_ModifyVectorStore = ModifyVectorStore
    { name = Nothing
    , expires_after = Nothing
    , metadata = Nothing
    }

-- | The status of the vector store
data Status = Expired | In_Progress | Completed
    deriving stock (Generic, Show)

instance FromJSON Status where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Status where
    toJSON = genericToJSON aesonOptions

-- | A vector store is a collection of processed files can be used by the
-- @file_search@ tool.
data VectorStoreObject = VectorStoreObject
    { id :: VectorStoreID
    , object :: Text
    , created_at :: POSIXTime
    , name :: Maybe Text
    , usage_bytes :: Natural
    , file_counts :: FileCounts
    , status :: Status
    , expires_after :: Maybe ExpiresAfter
    , expires_at :: Maybe POSIXTime
    , last_active_at :: Maybe POSIXTime
    , metadata :: Map Text Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
        "vector_stores"
    :>  Header' '[Required, Strict] "OpenAI-Beta" Text
    :>  (         ReqBody '[JSON] CreateVectorStore
              :>  Post '[JSON] VectorStoreObject
        :<|>      QueryParam "limit" Natural
              :>  QueryParam "order" Order
              :>  QueryParam "after" Text
              :>  QueryParam "before" Text
              :>  Get '[JSON] (ListOf VectorStoreObject)
        :<|>      Capture "vector_store_id" VectorStoreID
              :>  Get '[JSON] VectorStoreObject
        :<|>      Capture "vector_store_id" VectorStoreID
              :>  ReqBody '[JSON] ModifyVectorStore
              :>  Post '[JSON] VectorStoreObject
        :<|>      Capture "vector_store_id" VectorStoreID
              :>  Delete '[JSON] DeletionStatus
        )
