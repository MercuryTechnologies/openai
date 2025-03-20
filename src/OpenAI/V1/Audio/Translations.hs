-- | @\/v1\/audio\/translations@
--
-- To simplify things, this only supports the @verbose_json@ response format
module OpenAI.V1.Audio.Translations
    ( -- * Main types
      CreateTranslation(..)
    , _CreateTranslation
    , TranslationObject(..)
      -- * Servant
    , API
    ) where

import OpenAI.Prelude as OpenAI.Prelude
import OpenAI.V1.Models (Model(..))

import qualified Data.Text as Text

-- | Request body for @\/v1\/audio\/translations@
data CreateTranslation = CreateTranslation
    { file :: FilePath
    , model :: Model
    , prompt :: Maybe Text
    , temperature :: Maybe Double
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

instance ToMultipart Tmp CreateTranslation where
    toMultipart CreateTranslation{ model = Model model, ..} = MultipartData{..}
      where
        inputs =
                input "model" model
            <>  foldMap (input "prompt") prompt
            <>  input "response_format" "verbose_json"
            <>  foldMap (input "temperature" . renderRealFloat) temperature

        files = [ FileData{..} ]
          where
            fdInputName = "file"
            fdFileName = Text.pack file
            fdFileCType = "audio/" <> getExtension file
            fdPayload = file

-- | Default `CreateTranslation`
_CreateTranslation :: CreateTranslation
_CreateTranslation = CreateTranslation
    { prompt = Nothing
    , temperature = Nothing
    }

-- | Represents a transcription response returned by model, based on the
-- provided input.
data TranslationObject = TranslationObject
    { text :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
        "translations"
    :>  MultipartForm Tmp CreateTranslation
    :>  Post '[JSON] TranslationObject
