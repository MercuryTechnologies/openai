-- | @\/v1\/moderations@
module OpenAI.V1.Moderations
    ( -- * Main types
      CreateModeration(..)
    , _CreateModeration
    , Moderation(..)

      -- * Other types
    , InputType(..)
    , Result(..)

      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.Models (Model)

-- | Request body for @\/v1\/moderations@
data CreateModeration = CreateModeration
    { input :: Text
    , model :: Maybe Model
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `CreateModeration`
_CreateModeration :: CreateModeration
_CreateModeration = CreateModeration
    { model = Nothing
    }

-- | The input type that the score applies to
data InputType = Text | Image
    deriving stock (Generic, Show)

instance FromJSON InputType where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON InputType where
    toJSON = genericToJSON aesonOptions

-- | A moderation result
data Result = Result
    { flagged :: Bool
    , categories :: Map Text Bool
    , category_scores :: Map Text Double
    , category_applied_input_types :: Maybe (Map Text InputType)
    -- According to the OpenAPI spec the `category_applied_input_types`
    -- field is required but their actual implementation omits this field.
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Represents if a given text input is potentially harmful.
data Moderation = Moderation
    { id :: Text
    , model :: Model
    , results :: Vector Result
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
    "moderations" :> ReqBody '[JSON] CreateModeration :> Post '[JSON] Moderation
