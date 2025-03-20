-- | @\/v1\/vector_stores\/:vector_store_id\/files@
module OpenAI.V1.VectorStores.Files
    ( -- * Main types
      VectorStoreFileID(..)
    , CreateVectorStoreFile(..)
    , _CreateVectorStoreFile
    , VectorStoreFileObject(..)

      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.ChunkingStrategy
import OpenAI.V1.DeletionStatus
import OpenAI.V1.Files (FileID)
import OpenAI.V1.Error
import OpenAI.V1.ListOf
import OpenAI.V1.Order
import OpenAI.V1.VectorStores (VectorStoreID)
import OpenAI.V1.VectorStores.Status

-- | Vector store file ID
newtype VectorStoreFileID = VectorStoreFileID{ text :: Text }
    deriving newtype (FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Request body for @\/v1\/vector_stores\/:vector_store_id\/files@
data CreateVectorStoreFile = CreateVectorStoreFile
    { file_id :: FileID
    , chunking_strategy :: Maybe ChunkingStrategy
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `CreateVectorStoreFile`
_CreateVectorStoreFile :: CreateVectorStoreFile
_CreateVectorStoreFile = CreateVectorStoreFile
    { chunking_strategy = Nothing
    }

-- | A list of files attached to a vector store
data VectorStoreFileObject = VectorStoreFileObject
    { id :: VectorStoreFileID
    , object :: Text
    , usage_bytes :: Natural
    , created_at :: POSIXTime
    , vector_store_id :: VectorStoreID
    , status :: Status
    , last_error :: Maybe Error
    , chunking_strategy :: ChunkingStrategy
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
        "vector_stores"
    :>  Header' '[Required, Strict] "OpenAI-Beta" Text
    :>  (         Capture "vector_store_id" VectorStoreID
              :>  "files"
              :>  ReqBody '[JSON] CreateVectorStoreFile
              :>  Post '[JSON] VectorStoreFileObject
        :<|>      Capture "vector_store_id" VectorStoreID
              :>  "files"
              :>  QueryParam "limit" Natural
              :>  QueryParam "order" Order
              :>  QueryParam "after" Text
              :>  QueryParam "before" Text
              :>  QueryParam "filter" Status
              :>  Get '[JSON] (ListOf VectorStoreFileObject)
        :<|>      Capture "vector_store_id" VectorStoreID
              :>  "files"
              :>  Capture "file_id" VectorStoreFileID
              :>  Get '[JSON] VectorStoreFileObject
        :<|>      Capture "vector_store_id" VectorStoreID
              :>  "files"
              :>  Capture "file_id" VectorStoreFileID
              :>  Delete '[JSON] DeletionStatus
        )
