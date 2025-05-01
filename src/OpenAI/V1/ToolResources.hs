-- | The `ToolResources` type
module OpenAI.V1.ToolResources
    ( -- * Main types
      ToolResources(..)

      -- * Other types
    , CodeInterpreterResources(..)
    , Static(..)
    , VectorStore(..)
    , FileSearchResources(..)
    ) where

import OpenAI.Prelude
import OpenAI.V1.AutoOr
import OpenAI.V1.ChunkingStrategy
import OpenAI.V1.Files (FileID)

-- | Resources for the code search tool
data CodeInterpreterResources = CodeInterpreterResources
    { file_ids :: Maybe (Vector FileID)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | A helper to create a vector store with file_ids and attach it to this
-- assistant
data VectorStore = VectorStore
    { file_ids :: Maybe (Vector FileID)
    , chunking_strategy :: Maybe (AutoOr ChunkingStrategy)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Resources for the file search tool
data FileSearchResources = FileSearchResources
    { vector_store_ids :: Maybe (Vector FileID)
    , vector_stores :: Maybe (Vector VectorStore)
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON)

instance ToJSON FileSearchResources where
    toJSON = genericToJSON aesonOptions

-- | A set of resources that are used by the assistant's tools
data ToolResources = ToolResources
    { code_interpreter :: Maybe CodeInterpreterResources
    , file_search :: Maybe FileSearchResources
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)
