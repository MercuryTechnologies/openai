-- | @\/v1\/vector_stores\/:vector_store_id\/file_batches@
module OpenAI.V1.VectorStores.FileBatches
    ( -- * Main types
      VectorStoreFileBatchID(..)
    , CreateVectorStoreFileBatch(..)
    , _CreateVectorStoreFileBatch
    , VectorStoreFilesBatchObject(..)

      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.ChunkingStrategy
import OpenAI.V1.Files (FileID)
import OpenAI.V1.ListOf
import OpenAI.V1.Order
import OpenAI.V1.VectorStores (VectorStoreID)
import OpenAI.V1.VectorStores.FileCounts
import OpenAI.V1.VectorStores.Status

-- | Vector store file batch ID
newtype VectorStoreFileBatchID = VectorStoreFileBatchID{ text :: Text }
    deriving newtype (FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Request body for @\/v1\/vector_stores\/:vector_store_id\/file_batches@
data CreateVectorStoreFileBatch = CreateVectorStoreFileBatch
    { file_ids :: Vector FileID
    , chunking_strategy :: Maybe ChunkingStrategy
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `CreateVectorStoreFileBatch`
_CreateVectorStoreFileBatch :: CreateVectorStoreFileBatch
_CreateVectorStoreFileBatch = CreateVectorStoreFileBatch
    { chunking_strategy = Nothing
    }

-- | A batch of files attached to a vector store
data VectorStoreFilesBatchObject = VectorStoreFilesBatchObject
    { id :: VectorStoreFileBatchID
    , object :: Text
    , created_at :: POSIXTime
    , vector_store_id :: VectorStoreID
    , status :: Status
    , file_counts :: Maybe FileCounts
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
        "vector_stores"
    :>  Header' '[Required, Strict] "OpenAI-Beta" Text
    :>  (         Capture "vector_store_id" VectorStoreID
              :>  "file_batches"
              :>  ReqBody '[JSON] CreateVectorStoreFileBatch
              :>  Post '[JSON] VectorStoreFilesBatchObject
        :<|>      Capture "vector_store_id" VectorStoreID
              :>  "file_batches"
              :>  Capture "batch_id" VectorStoreFileBatchID
              :>  Get '[JSON] VectorStoreFilesBatchObject
        :<|>      Capture "vector_store_id" VectorStoreID
              :>  "file_batches"
              :>  Capture "batch_id" VectorStoreFileBatchID
              :>  "cancel"
              :>  Post '[JSON] VectorStoreFilesBatchObject
        :<|>      Capture "vector_store_id" VectorStoreID
              :>  "file_batches"
              :>  Capture "batch_id" VectorStoreFileBatchID
              :>  "files"
              :>  QueryParam "limit" Natural
              :>  QueryParam "order" Order
              :>  QueryParam "after" Text
              :>  QueryParam "before" Text
              :>  QueryParam "filter" Status
              :>  Get '[JSON] (ListOf VectorStoreFilesBatchObject)
        )
