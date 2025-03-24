-- | The `DeletionStatus` type
module OpenAI.V1.DeletionStatus
    ( -- * Types
      DeletionStatus(..)
    ) where

import OpenAI.Prelude

-- | Deletion status
data DeletionStatus = DeletionStatus
    { id :: Text
    , object :: Text
    , deleted :: Bool
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)
