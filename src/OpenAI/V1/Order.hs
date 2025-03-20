-- | The `Order` type
module OpenAI.V1.Order
    ( -- * Types
      Order(..)
    ) where

import OpenAI.Prelude

-- | Sort order by the @created_at@ timestamp of the objects
data Order = Desc | Asc
    deriving stock (Generic, Show)

instance FromJSON Order where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Order where
    toJSON = genericToJSON aesonOptions

instance ToHttpApiData Order where
    toUrlPiece Desc = "desc"
    toUrlPiece Asc = "asc"
