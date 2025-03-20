-- | @\/v1\/images\/variations@
module OpenAI.V1.Images.Variations
    ( -- * Main types
      CreateImageVariation(..)
    , _CreateImageVariation
      -- * Servant
    , API
    ) where

import OpenAI.Prelude
import OpenAI.V1.Images.Image
import OpenAI.V1.Images.ResponseFormat
import OpenAI.V1.ListOf
import OpenAI.V1.Models (Model(..))

import qualified Data.Text as Text

-- | Request body for @\/v1\/images\/variations@
data CreateImageVariation = CreateImageVariation
    { image :: FilePath
    , model :: Maybe Model
    , n :: Maybe Natural
    , response_format :: Maybe ResponseFormat
    , size :: Maybe Text
    , user :: Maybe Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `CreateImageVariation`
_CreateImageVariation :: CreateImageVariation
_CreateImageVariation = CreateImageVariation
    { model = Nothing
    , n = Nothing
    , response_format = Nothing
    , size = Nothing
    , user = Nothing
    }

instance ToMultipart Tmp CreateImageVariation where
    toMultipart CreateImageVariation{..} = MultipartData{..}
      where
        inputs =
                foldMap (input "model" . text) model
            <>  foldMap (input "n" . renderIntegral) n
            <>  foldMap (input "response_format" . toUrlPiece) response_format
            <>  foldMap (input "size") size
            <>  foldMap (input "user") user

        files = [ FileData{..} ]
          where
            fdInputName = "image"
            fdFileName = Text.pack image
            fdFileCType = "image/" <> getExtension image
            fdPayload = image

-- | Servant API
type API =
        "variations"
    :>  MultipartForm Tmp CreateImageVariation
    :>  Post '[JSON] (ListOf ImageObject)
