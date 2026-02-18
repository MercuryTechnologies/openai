-- | The `ResponseFormat` type
module OpenAI.V1.ResponseFormat
    ( -- * Types
      ResponseFormat(..)
    , JSONSchema(..)
    ) where

import OpenAI.Prelude

-- | An object specifying the format that the model must output
data ResponseFormat
    = ResponseFormat_Text
    | JSON_Object
    | JSON_Schema{ json_schema :: JSONSchema }
    deriving stock (Generic, Show)

responseFormatOptions :: Options
responseFormatOptions = aesonOptions
    { sumEncoding =
        TaggedObject{ tagFieldName = "type", contentsFieldName = "" }

    , tagSingleConstructors = True

    , constructorTagModifier = stripPrefix "ResponseFormat_"
    }

instance FromJSON ResponseFormat where
    parseJSON = genericParseJSON responseFormatOptions

instance ToJSON ResponseFormat where
    toJSON = genericToJSON responseFormatOptions

-- | Setting to { "type": "json_schema", "json_schema": {...} } enables
-- Structured Outputs which ensures the model will match your supplied JSON
-- schema. Learn more in the
-- [Structured Outputs](https://platform.openai.com/docs/guides/structured-outputs) guide
data JSONSchema = JSONSchema
    { description :: Maybe Text
    , name :: Text
    , schema :: Maybe Value
    , strict :: Maybe Bool
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)
