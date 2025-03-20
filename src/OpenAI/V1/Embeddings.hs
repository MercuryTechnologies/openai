-- | @\/v1\/embeddings@
module OpenAI.V1.Embeddings
    ( -- * Main types
      CreateEmbeddings(..)
    , _CreateEmbeddings
    , EmbeddingObject(..)
      -- * Other types
    , EncodingFormat(..)
      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.ListOf
import OpenAI.V1.Models (Model)

-- | The format to return the embeddings in
data EncodingFormat = Float | Base64
    deriving stock (Generic, Show)

instance FromJSON EncodingFormat where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON EncodingFormat where
    toJSON = genericToJSON aesonOptions

-- | Request body for @\/v1\/embeddings@
data CreateEmbeddings = CreateEmbeddings
    { input :: Text
    , model :: Model
    , encoding_format :: Maybe EncodingFormat
    , dimensions :: Maybe Natural
    , user :: Maybe Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `CreateEmbeddings`
_CreateEmbeddings :: CreateEmbeddings
_CreateEmbeddings = CreateEmbeddings
    { encoding_format = Nothing
    , dimensions = Nothing
    , user = Nothing
    }

-- | The embedding object
data EmbeddingObject = EmbbeddingObject
    { index :: Natural
    , embedding :: Vector Double
    , object :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
        "embeddings"
    :>  ReqBody '[JSON] CreateEmbeddings
    :>  Post '[JSON] (ListOf EmbeddingObject)
