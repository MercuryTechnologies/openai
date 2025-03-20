-- | The response format
module OpenAI.V1.Images.ResponseFormat
    ( -- * Types
      ResponseFormat(..)
    ) where

import OpenAI.Prelude

-- | The format in which the generated images are returned
data ResponseFormat = URL | B64_JSON
    deriving stock (Generic, Show)

instance FromJSON ResponseFormat where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON ResponseFormat where
    toJSON = genericToJSON aesonOptions

instance ToHttpApiData ResponseFormat where
    toUrlPiece URL = "url"
    toUrlPiece B64_JSON = "b64_json"
