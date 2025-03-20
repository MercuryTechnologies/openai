-- | @\/v1\/images\/generations@
module OpenAI.V1.Images.Generations
    ( -- * Main types
      CreateImage(..)
    , _CreateImage
      -- * Other types
    , Quality(..)
    , Style(..)
      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.Images.Image
import OpenAI.V1.Images.ResponseFormat
import OpenAI.V1.ListOf
import OpenAI.V1.Models (Model)

-- | The quality of the image that will be generated
data Quality = Standard | HD
    deriving stock (Generic, Show)

instance FromJSON Quality where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Quality where
    toJSON = genericToJSON aesonOptions

-- | The style of the generated images
data Style = Vivid | Natural
    deriving stock (Generic, Show)

instance FromJSON Style where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Style where
    toJSON = genericToJSON aesonOptions

-- | Requesty body for @\/v1/images/generations@
data CreateImage = CreateImage
    { prompt :: Text
    , model :: Maybe Model
    , n :: Maybe Natural
    , quality :: Maybe Quality
    , response_format :: Maybe ResponseFormat
    , size :: Maybe Text
    , style :: Maybe Style
    , user :: Maybe Text
    } deriving stock (Generic, Show)

-- | Default `CreateImage`
_CreateImage :: CreateImage
_CreateImage = CreateImage
    { model = Nothing
    , n = Nothing
    , quality = Nothing
    , response_format = Nothing
    , size = Nothing
    , style = Nothing
    , user = Nothing
    }

instance FromJSON CreateImage where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON CreateImage where
    toJSON = genericToJSON aesonOptions

-- | Servant API
type API =
        "generations"
    :>  ReqBody '[JSON] CreateImage
    :>  Post '[JSON] (ListOf ImageObject)
