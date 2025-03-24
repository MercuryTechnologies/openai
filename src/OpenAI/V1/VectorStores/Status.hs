-- | The `Status` type
module OpenAI.V1.VectorStores.Status
    ( -- * Main types
      Status(..)
    ) where

import OpenAI.Prelude

-- | The status of the vector store file
data Status = In_Progress | Completed | Cancelled | Failed
    deriving stock (Generic, Show)

instance FromJSON Status where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Status where
    toJSON = genericToJSON aesonOptions

instance ToHttpApiData Status where
    toUrlPiece In_Progress = "in_progress"
    toUrlPiece Completed = "completed"
    toUrlPiece Cancelled = "cancelled"
    toUrlPiece Failed = "failed"
