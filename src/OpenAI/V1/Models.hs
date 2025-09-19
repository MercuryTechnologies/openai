-- | @\/v1\/models@
module OpenAI.V1.Models
  ( -- * Main types
    Model (..),
    ModelObject (..),

    -- * Servant
    API,
  )
where

import OpenAI.Prelude
import OpenAI.V1.DeletionStatus
import OpenAI.V1.ListOf

-- | Model
newtype Model = Model {text :: Text}
  deriving newtype (FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Describes an OpenAI model offering that can be used with the API
data ModelObject = ModelObject
  { id :: Model
  , created :: POSIXTime
  , object :: Text
  , owned_by :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- | Servant API
type API =
  "models"
    :> ( Get '[JSON] (ListOf ModelObject)
          :<|> Capture "model" Model
          :> Get '[JSON] ModelObject
          :<|> Capture "model" Model
          :> Delete '[JSON] DeletionStatus
       )
