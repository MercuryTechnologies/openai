-- | The `ListOf` type constructor
module OpenAI.V1.ListOf
    ( -- * Types
      ListOf(..)
    ) where

import OpenAI.Prelude

-- | Whenever OpenAI says that an API endpoint returns a list of items, what
-- they actually mean is that it returns that list wrapped inside of the
-- following object
data ListOf a = List{ data_ :: Vector a }
    deriving stock (Generic, Show)

instance FromJSON a => FromJSON (ListOf a) where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON a => ToJSON (ListOf a) where
    toJSON = genericToJSON aesonOptions
